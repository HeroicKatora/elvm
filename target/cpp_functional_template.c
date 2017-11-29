#include <ir/ir.h>
#include <target/util.h>

typedef struct {
  int nr_register;
  int nr_pc;
  int nr_memory;
  int nr_stdin;
  int nr_stdout;
} FuncState;

typedef struct {
   // A register or Unsigned<?>, fitting up to 10 digits integers
  char buffer[21];
} ValueBuffer;

static FuncState func_state;
static const FuncState initial_state = { .nr_register = 0, .nr_pc = 0,
  .nr_memory = 0, .nr_stdin = 0, .nr_stdout = 0 };

// The header at the top of the file, where the library and stdin is set up.
static void emit_readable_setup(void);

// Necessary constructions for the program part
static void emit_program_init(void);
static void emit_program_runloop(void);

static void cpp_emit_pcblocks(Inst* inst);
static void cpp_emit_data(Data* data);
static void cpp_emit_block_prologue(int pc);
static void cpp_emit_block_epilogue(void);
static void cpp_emit_inst(Inst* inst);

void target_cpp_functional_template(Module* module) {
  func_state = initial_state;

  emit_readable_setup();
  emit_program_init();

  cpp_emit_data(module->data);
  cpp_emit_pcblocks(module->text);

  emit_program_runloop();
}

void cpp_emit_pcblocks(Inst* inst) {
  int prev_pc = -1;
  for(; inst; inst = inst->next) {
    if(prev_pc != inst->pc) {
      if(prev_pc != -1) {
        cpp_emit_block_epilogue();
      }
      cpp_emit_block_prologue(inst->pc);
    }
    prev_pc = inst->pc;
    cpp_emit_inst(inst);
  }
  cpp_emit_block_epilogue();
}

void cpp_emit_block_prologue(int pc) {
  emit_line(
    "template<\n"
    "  typename Registers0, typename Memory0, typename Stdin0, typename Stdout0>\n"
    "struct RunFunc<State<Registers0, Unsigned<%i>, Memory0, Stdin0, Stdout0>> {\n"
    "  using Pc0 = Unsigned<(%i + 1)>;", pc, pc);
}

void cpp_emit_block_epilogue(void) {
  emit_line(
    "  using type = State<Registers%i, Pc%i, Memory%i, Stdin%i, Stdout%i>;"
    " };", func_state.nr_register, func_state.nr_pc, func_state.nr_memory,
    func_state.nr_stdin, func_state.nr_stdout);
  func_state = initial_state;
}

static ValueBuffer cpp_print_value(const Value value) {
  ValueBuffer buffer;
  switch(value.type) {
  case REG: {
    const char *regid;
    switch(value.reg) {
      case A: regid = "A"; break;
      case B: regid = "B"; break;
      case C: regid = "C"; break;
      case D: regid = "D"; break;
      case BP: regid = "BP"; break;
      case SP: regid = "SP"; break;
      default:
        error("Unexpected register %i, invalid eir", value.reg);
    }
    snprintf(buffer.buffer, sizeof(buffer.buffer), "%s", regid);
  } break;
  case IMM:
    if(value.imm < 0) {
      error("Unexpected signed integral %i", value.imm);
    } else {
      snprintf(buffer.buffer, sizeof(buffer.buffer), "Unsigned<%u>", value.imm);
    }
  }
  return buffer;
}

void cpp_emit_inst(Inst* inst) {
  ValueBuffer dst, src, jmp;
  switch(inst->op) {
    // Functions operation only on registers
  case MOV: // fallthrough
  case ADD: // fallthrough
  case SUB: // fallthrough
  case EQ: // fallthrough
  case NE: // fallthrough
  case LT: // fallthrough
  case GT: // fallthrough
  case LE: // fallthrough
  case GE: {
    const char* function;
    if (inst->op >= MOV && inst->op <= SUB) {
      const char* fn_names[] = {"mov", "add", "sub"};
      function = fn_names[inst->op - MOV];
    } else if (inst->op >= EQ && inst->op <= GE) {
      const char* fn_names[] = {"eq", "ne", "lt", "gt", "le", "ge"};
      function = fn_names[inst->op - EQ];
    } else {
      error("Logic error, unexpected instruction fallthrough");
    }
    dst = cpp_print_value(inst->dst);
    src = cpp_print_value(inst->src);
    emit_line(
      "  using Registers%i = Apply<%s, %s, %s, Registers%i>;",
      func_state.nr_register + 1,
      function,
      dst.buffer,
      src.buffer,
      func_state.nr_register);
    func_state.nr_register++;
  } break;
    // Memory - Register communication
  case LOAD: {
    dst = cpp_print_value(inst->dst);
    src = cpp_print_value(inst->src);
    emit_line(
      "  using Registers%i = Apply<load, %s, %s, Registers%i, Memory%i>;",
      func_state.nr_register + 1,
      dst.buffer,
      src.buffer,
      func_state.nr_register,
      func_state.nr_memory);
    func_state.nr_register++;
  } break;
  case STORE: {
    dst = cpp_print_value(inst->dst);
    src = cpp_print_value(inst->src);
    emit_line(
      "  using Memory%i = Apply<store, %s, %s, Registers%i, Memory%i>;",
      func_state.nr_memory + 1,
      src.buffer,
      dst.buffer,
      func_state.nr_register,
      func_state.nr_memory);
    func_state.nr_memory++;
  } break;
    // IO
  case PUTC: {
    dst = cpp_print_value(inst->dst);
    emit_line(
      "  using Stdout%i = Apply<putcop, %s, Registers%i, Stdout%i>;",
      func_state.nr_stdout + 1,
      dst.buffer,
      func_state.nr_register,
      func_state.nr_stdout);
    func_state.nr_stdout++;
  } break;
  case GETC: {
    dst = cpp_print_value(inst->dst);
    emit_line(
      "  using Registers%i = Apply<peek, %s, Registers%i, Stdin%i>;",
      func_state.nr_register + 1,
      dst.buffer,
      func_state.nr_register,
      func_state.nr_stdin);
    emit_line(
      "  using Stdin%i = Apply<advance, Stdin%i>;",
      func_state.nr_stdin + 1,
      func_state.nr_stdin);
    func_state.nr_register++;
    func_state.nr_stdin++;
  } break;
    // Program flow via jumps
  case JEQ: // falltrough
  case JNE: // falltrough
  case JLT: // falltrough
  case JGT: // falltrough
  case JLE: // falltrough
  case JGE: {
    dst = cpp_print_value(inst->dst);
    src = cpp_print_value(inst->src);
    jmp = cpp_print_value(inst->jmp);
    const char* compare[] =
      {"jmpeq", "jmpne", "jmplt", "jmpgt", "jmple", "jmpge"};
    emit_line(
      "  using Pc%i = Apply<%s, %s, %s, %s, Registers%i, Pc%i>;",
      func_state.nr_pc + 1,
      compare[inst->op - JEQ],
      jmp.buffer,
      dst.buffer,
      src.buffer,
      func_state.nr_register,
      func_state.nr_pc);
    func_state.nr_pc++;
  } break;
  case JMP: {
    jmp = cpp_print_value(inst->jmp);
    emit_line(
      "  using Pc%i = Apply<jmp, %s, Registers%i>;",
      func_state.nr_pc + 1,
      jmp.buffer,
      func_state.nr_register);
    func_state.nr_pc++;
  } break;
  case EXIT: {
    emit_line(
      "  using Pc%i = Exit;",
      ++func_state.nr_pc);
  } break;
    // Ignorable instructions
  case DUMP: break;
  default: break;
    error("Unexpected instruction");
  }
}

void emit_readable_setup(void) {
  emit_line(
    "#include <hdr/elvm/core.hpp>\n"
    "using namespace ::hdr::elvm;\n"
    "namespace io {\n"
    "  // Replace this with the input before running\n"
    "  constexpr static char stdin [] = \"int main() {}\";\n"
    "}");
}

void emit_program_init(void) {
  emit_line(
    "using Input = Stdin<io::stdin>;\n"
    "template<\n"
    "  typename Registers,\n"
    "  typename Pc,\n"
    "  typename Memory,\n"
    "  typename Stdin,\n"
    "  typename Stdout>\n"
    "struct State;\n"
    "template<typename State> struct RunFunc;\n"
    "using runfunc = TypeFunction<RunFunc>;");
  emit_line(
    "template<\n"
    "  typename Registers0, typename Memory0, typename Stdin0, typename Stdout0>\n"
    "struct RunFunc<State<Registers0, Exit, Memory0, Stdin0, Stdout0>> {\n"
    "  using type = Stdout0;\n"
    "};");
}

void cpp_emit_data(Data* data) {
  emit_line("using InitMemory = Apply<memory, Array<");
  printf("  ");
  Data* prev = NULL;
  for(; data; data = data->next) {
    if(prev) printf(",");
    printf("Unsigned<%i>", data->v);
    prev = data;
  }
  emit_line(">>;");
}

void emit_program_runloop(void) {
  emit_line(
    "using ::hdr::match::_;\n"
    "using InitState = State<Registers<>, Unsigned<0>, InitMemory, Input, Stdout>;\n"
    "using TerminatePattern = State<_, Exit, _, _, _>;\n"
    "using exit_test = ::hdr::match::MatchClause<\n"
    "  ::hdr::match::With<TerminatePattern, Const<True>>,\n"
    "  ::hdr::match::With<_, Const<False>>>;\n"
    "using FinalState = Apply<until, exit_test, runfunc, InitState>;\n"
    "using FinalStdout = Apply<runfunc, FinalState>;\n"
    "using Output = Apply<collect, FinalStdout>;\n"
    "int main() { printf(\"%%s\", Output::buffer); }");
}
