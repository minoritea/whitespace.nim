import std/strformat, std/tables, std/parseutils

type
  #[
  IMP = enum
    StackManipulation,
    Arithmetic,
    HeapAccess,
    FlowControl,
    IO
  ]#

  Op = enum
    Push,
    Duplicate,
    Copy,
    Swap,
    Discard,
    Slide,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Store,
    Retrieve,
    Label,
    Call,
    Jump,
    JumpIfZero,
    JumpIfNegative,
    Return,
    End,
    OutputChar,
    OutputNumber,
    ReadChar,
    ReadNumber

  Command = ref object
    case op: Op
    of Push, Copy, Slide:
      value: int
    of Duplicate, Swap, Discard:
      nil
    of Add, Subtract, Multiply, Divide, Modulo:
      nil
    of Store, Retrieve:
      nil
    of Label, Call, Jump, JumpIfZero, JumpIfNegative:
      label: int
    of Return, End:
      nil
    of OutputChar, OutputNumber, ReadChar, ReadNumber:
      nil

  ParseMode = enum
    ParseCommand,
    ParseNumber,
    ParseLabel

proc `$`(command: Command): string =
    result.add &"Op: {$command.op}"
    case command.op
    of Push, Copy, Slide:
      result.add &", Value: {$command.value}"
    of Label, Call, Jump, JumpIfZero, JumpIfNegative:
      result.add &", Label: {$command.label}"
    else:
      discard

proc whitespaceToInt(s: string): int =
  result = 0
  for i, c in s:
    case c
    of ' ':
      result = result shl 1
    of '\t':
      result = (result shl 1) or 1
    of '\n':
      return result
    else:
      continue
  raise newException(ValueError, "No integer termination")

proc parse(code: string): seq[Command] =
  var mode = ParseCommand
  var buf = ""
  var op: Op
  for c in code:
    if not (c in " \t\n"):
      continue
    buf.add c
    case mode
    of ParseCommand:
        case buf
        of "  ":
          op = Push
          mode = ParseNumber
          buf = ""
        of " \n ":
          result.add Command(op: Duplicate)
          mode = ParseCommand
          buf = ""
        of " \t ":
          op = Copy
          mode = ParseNumber
          buf = ""
        of " \n\t":
          result.add Command(op: Swap)
          mode = ParseCommand
          buf = ""
        of " \n\n":
          result.add Command(op: Discard)
          mode = ParseCommand
          buf = ""
        of " \t\n":
          op = Slide
          mode = ParseNumber
          buf = ""
        of "\t   ":
          result.add Command(op: Add)
          mode = ParseCommand
          buf = ""
        of "\t  \t":
          result.add Command(op: Subtract)
          mode = ParseCommand
          buf = ""
        of "\t  \n":
          result.add Command(op: Multiply)
          mode = ParseCommand
          buf = ""
        of "\t \t ":
          result.add Command(op: Divide)
          mode = ParseCommand
          buf = ""
        of "\t \t\t":
          result.add Command(op: Modulo)
          mode = ParseCommand
          buf = ""
        of "\t\t ":
          result.add Command(op: Store)
          mode = ParseCommand
          buf = ""
        of "\t\t\t":
          result.add Command(op: Retrieve)
          mode = ParseCommand
          buf = ""
        of "\n  ":
          op = Label
          mode = ParseLabel
          buf = ""
        of "\n \t":
          op = Call
          mode = ParseLabel
          buf = ""
        of "\n \n":
          op = Jump
          mode = ParseLabel
          buf = ""
        of "\n\t ":
          op = JumpIfZero
          mode = ParseLabel
          buf = ""
        of "\n\t\t":
          op = JumpIfNegative
          mode = ParseLabel
          buf = ""
        of "\n\t\n":
          result.add Command(op: Return)
          mode = ParseCommand
          buf = ""
        of "\n\n\n":
          result.add Command(op: End)
          mode = ParseCommand
          buf = ""
        of "\t\n  ":
          result.add Command(op: OutputChar)
          mode = ParseCommand
          buf = ""
        of "\t\n \t":
          result.add Command(op: OutputNumber)
          mode = ParseCommand
          buf = ""
        of "\t\n\t ":
          result.add Command(op: ReadChar)
          mode = ParseCommand
          buf = ""
        of "\t\n\t\t":
          result.add Command(op: ReadNumber)
          mode = ParseCommand
          buf = ""
        else:
          if buf.len < 4:
            continue
          let chars = cast[seq[char]](buf)
          raise newException(ValueError, &"invalid command sequence: {$chars}")
    of ParseNumber:
      case c
      of '\n':
        case op
        of Push:
          result.add Command(op: Push, value: buf.whitespaceToInt)
        of Copy:
          result.add Command(op: Copy, value: buf.whitespaceToInt)
        of Slide:
          result.add Command(op: Slide, value: buf.whitespaceToInt)
        else: raise newException(ValueError, "Invalid Stack Manipulation Command")
        mode = ParseCommand
        buf = ""
      else:
        continue
    of ParseLabel:
      case c
      of '\n':
        case op
        # of Label, Call, Jump, JumpIfZero, JumpIfNegative:
        of Label:
          result.add Command(op: Label, label: buf.whitespaceToInt)
        of Call:
          result.add Command(op: Call, label: buf.whitespaceToInt)
        of Jump:
          result.add Command(op: Jump, label: buf.whitespaceToInt)
        of JumpIfZero:
          result.add Command(op: JumpIfZero, label: buf.whitespaceToInt)
        of JumpIfNegative:
          result.add Command(op: JumpIfNegative, label: buf.whitespaceToInt)
        else: raise newException(ValueError, "Invalid Flow Control Command")
        mode = ParseCommand
        buf = ""
      else:
        continue

proc exec(commands: seq[Command]) =
  var stack = newSeq[int]()
  var heap = newTable[int, int]()
  var label = newTable[int, int]()
  var callStack = newSeq[int]()
  for i, command in commands:
    case command.op
    of Label:
      label[command.label] = i
    else:
      discard
  
  var i = 0
  var maxCommands = commands.len
  while i < maxCommands:
    let command = commands[i]
    case command.op
    of Push:
      stack.add command.value
    of Duplicate:
      if stack.len < 1:
        raise newException(ValueError, "stack is too small to duplicate")
      stack.add stack[^1]
    of Copy:
      let high = stack.high
      let low = high - command.value
      if low < 0:
        raise newException(ValueError, "underflow copy index")
      if low > high:
        raise newException(ValueError, "overflow copy index")
      stack.add stack[low..high]
    of Swap:
      if stack.len < 2:
        raise newException(ValueError, "stack is too small to swap")
      swap(stack[^1], stack[^2])
    of Discard:
      if stack.len < 1:
        raise newException(ValueError, "stack is too small to discard")
      discard stack.pop
    of Slide:
      let high = stack.high
      let low = high - command.value
      if low < 0:
        raise newException(ValueError, "underflow slide index")
      if low > high:
        raise newException(ValueError, "overflow slide index")
      swap(stack[^1], stack[low])
      stack.setLen(low)
    of Add:
      if stack.len < 2:
        raise newException(ValueError, "stack is too small to add")
      stack[^2] += stack[^1]
      discard stack.pop
    of Subtract:
      if stack.len < 2:
        raise newException(ValueError, "stack is too small to subtract")
      stack[^2] -= stack[^1]
      discard stack.pop
    of Multiply:
      if stack.len < 2:
        raise newException(ValueError, "stack is too small to multiply")
      stack[^2] *= stack[^1]
      discard stack.pop
    of Divide:
      if stack.len < 2:
        raise newException(ValueError, "stack is too small to divide")
      stack[^2] = stack[^2] div stack[^1]
      discard stack.pop
    of Modulo:
      if stack.len < 2:
        raise newException(ValueError, "stack is too small to modulo")
      stack[^2] = stack[^2] mod stack[^1]
      discard stack.pop
    of Store:
      if stack.len < 2:
        raise newException(ValueError, "stack is too small to store")
      heap[stack[^2]] = stack[^1]
      stack.setLen(stack.high - 2)
    of Retrieve:
      if stack.len < 1:
        raise newException(ValueError, "stack is too small to retrieve")
      stack[^1] = heap[stack[^1]]
    of Label:
      discard
    of Call:
      if not label.hasKey(command.label):
        raise newException(ValueError, "invalid call label")
      i = label[command.label]
      callstack.add i
      continue
    of Jump:
      if not label.hasKey(command.label):
        raise newException(ValueError, "invalid jump label")
      i = label[command.label]
      continue
    of JumpIfZero:
      if stack.len < 1:
        raise newException(ValueError, "stack is too small to jump if zero")
      if stack[^1] == 0:
        i = label[command.label]
        continue
    of JumpIfNegative:
      if stack.len < 1:
        raise newException(ValueError, "stack is too small to jump if negative")
      if stack[^1] < 0:
        i = label[command.label]
        continue
    of Return:
      if callStack.len < 1:
        raise newException(ValueError, "stack is too small to return")
      i = callStack.pop
      continue
    of End:
      quit()
    of OutputChar:
      if stack.len < 1:
        raise newException(ValueError, "stack is too small to output char")
      stdout.write stack[^1].char
    of OutputNumber:
      if stack.len < 1:
        raise newException(ValueError, "stack is too small to output number")
      stdout.write $stack[^1]
    of ReadChar:
      stack.add stdin.readChar.int
    of ReadNumber:
      var num: int
      if parseInt([stdin.readChar], num) == 0:
        raise newException(ValueError, "invalid number")
      stack.add num
    i.inc

let commands = parse(stdin.readAll())
echo commands
commands.exec
