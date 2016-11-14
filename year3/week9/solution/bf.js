var readline = require('readline');

function buildJumpTable(code) {
  var jumpTable            = {},
      bracketLocationStack = [];

  code.split('').forEach((command, index) => {
    if (command == "[") {
      bracketLocationStack.push(index);
    } else if (command == "]") {
      var openBracket = bracketLocationStack.pop();
      jumpTable[openBracket] = index;
      jumpTable[index] = openBracket;
    }
  });

  return jumpTable;
}

function interpret(code) {
  var jumpTable          = buildJumpTable(code),
      memory             = new Array(2048).fill(0),
      instructionPointer = 0,
      memoryPointer      = 0;

  var instructionHandlers = {
    ">": () => ++memoryPointer,
    "<": () => --memoryPointer,
    "+": () => ++memory[memoryPointer],
    "-": () => --memory[memoryPointer],
    ".": () => process.stdout.write(String.fromCharCode(memory[memoryPointer])),
    "[": () => {
      if (memory[memoryPointer] == 0) {
        instructionPointer = jumpTable[instructionPointer];
      }
    },
    "]": () => {
      if (memory[memoryPointer] != 0) {
        instructionPointer = jumpTable[instructionPointer];
      }
    }
  }

  while (instructionPointer != code.length) {
    var currentInstruction = code[instructionPointer];
    instructionHandlers[currentInstruction]();
    ++instructionPointer;
  }
}

var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

rl.on('line', interpret);
