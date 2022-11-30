const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const Primitives = @import("primitives.zig");
const Opcode = Primitives.Opcode;
const Token = Primitives.Token;
const Instruction = Primitives.Instruction;
var LOAD_CONST = Primitives.LOAD_CONST;
var STORE_NAME = Primitives.STORE_NAME;
var LOAD_NAME = Primitives.LOAD_NAME;
var CALL_FUNCTION = Primitives.CALL_FUNCTION;
var MAKE_FUNCTION = Primitives.MAKE_FUNCTION;
var UNARY_NEGATIVE = Primitives.UNARY_NEGATIVE;
var UNARY_NOT = Primitives.UNARY_NOT;
var RELATIVE_JUMP = Primitives.RELATIVE_JUMP;
var RELATIVE_JUMP_IF_TRUE = Primitives.RELATIVE_JUMP_IF_TRUE;
var COPY = Primitives.COPY;

const Parser = @import("parser.zig");
const parse = Parser.parse;
const OPEN_EXP = Parser.OPEN_EXP;
const CLOSE_EXP = Parser.CLOSE_EXP;
const VAL = Parser.VAL;
const IF = Parser.IF;
const TIMES = Parser.TIMES;
const LAMBDA = Parser.LAMBDA;

pub const CompilerError = error {
    OutOfMemory,
    NotImplemented,
    InvalidSyntax
};

pub fn compile(allocator: Allocator, tokens: []Token) CompilerError![]Instruction {
    var instructions = ArrayList(Instruction).init(allocator);
    errdefer instructions.deinit();

    var i: usize = 0;
    while(i < tokens.len) {
        i += try compileExpression(tokens, &instructions, i);

        if(i >= tokens.len) break;
    }

    return instructions.toOwnedSlice();

}

fn compileExpression(tokens: []Token, instructions: *ArrayList(Instruction), offset: usize) CompilerError!usize {
    var tokensConsumed: usize = 1; // assume first token consumed

    if(offset >= tokens.len) return CompilerError.InvalidSyntax;

    switch(tokens[offset]) {
        .integer => try instructions.append(LOAD_CONST.call(tokens[offset])),
        .string => |s| {
            if(std.mem.eql(u8, s, VAL.string)) {
                if(tokens[offset + 1] != Token.string) return CompilerError.InvalidSyntax;
                var name = tokens[offset + 1];
                tokensConsumed += 1;

                tokensConsumed += try compileExpression(tokens, instructions, offset + 2);

                try instructions.append(STORE_NAME.call(name));
            } else if(std.mem.eql(u8, s, OPEN_EXP.string)) {
                // Function name
                var name = tokens[offset + 1];
                tokensConsumed += 1;

                if(std.mem.eql(u8, name.string, LAMBDA.string)) {
                    // Params
                    if(std.meta.eql(OPEN_EXP, tokens[offset + tokensConsumed + 1])) return CompilerError.InvalidSyntax;
                    tokensConsumed += 1;

                    var arguments = ArrayList(Token).init(instructions.allocator);
                    defer arguments.deinit();
                    while(true) {
                        var token = tokens[offset + tokensConsumed];
                        if(std.meta.eql(CLOSE_EXP, token)) {
                            // End of Params
                            tokensConsumed += 1;
                            break;
                        }

                        if(token != Token.string) return CompilerError.InvalidSyntax;

                        try arguments.append(token);
                        tokensConsumed += 1;
                    }
                    var arity = arguments.items.len;
                    try instructions.append(LOAD_CONST.call(Token{ .tuple = arguments.toOwnedSlice() }));

                    // Body
                    var body = ArrayList(Instruction).init(instructions.allocator);
                    defer body.deinit();
                    tokensConsumed += try compileExpression(tokens, &body, offset + tokensConsumed);
                    
                    try instructions.append(LOAD_CONST.call(Token{ .instructions = body.toOwnedSlice() }));
                    tokensConsumed += 1;

                    // Make function
                    try instructions.append(MAKE_FUNCTION.call(Token{ .integer = @intCast(i32, arity) }));
                } else if(std.mem.eql(u8, name.string, VAL.string)) {
                    if(tokens[offset + 2] != Token.string) return CompilerError.InvalidSyntax;
                    var var_name = tokens[offset + 2];
                    tokensConsumed += 1;

                    tokensConsumed += try compileExpression(tokens, instructions, offset + 3);

                    try instructions.append(STORE_NAME.call(var_name));
                } else if(std.mem.eql(u8, name.string, IF.string)) {
                    // cond
                    tokensConsumed += try compileExpression(tokens, instructions, offset + tokensConsumed);

                    // if(true) - code
                    var if_true_instructions = ArrayList(Instruction).init(instructions.allocator);
                    defer if_true_instructions.deinit();
                    tokensConsumed += try compileExpression(tokens, &if_true_instructions, offset + tokensConsumed);

                    // if(false) - code
                    var if_false_instructions = ArrayList(Instruction).init(instructions.allocator);
                    defer if_false_instructions.deinit();
                    tokensConsumed += try compileExpression(tokens, &if_false_instructions, offset + tokensConsumed);
                    try if_false_instructions.append(RELATIVE_JUMP.call(Token{ .integer = @intCast(i32, if_false_instructions.items.len) }));

                    // push to stack
                    try instructions.append(RELATIVE_JUMP_IF_TRUE.call(Token{ .integer = @intCast(i32, if_false_instructions.items.len) }));
                    for(if_false_instructions.items) |ins| 
                        try instructions.append(ins);
                    for(if_true_instructions.items) |ins| 
                        try instructions.append(ins);

                    tokensConsumed += 1; // )
                } else { // Function call
                    try instructions.append(LOAD_NAME.call(name));

                    // Arguments
                    var argumentOffset: usize = offset + tokensConsumed;
                    var fnArity: usize = 0;
                    while(true) {
                        var token = tokens[argumentOffset];
                        if(token == Token.string and std.mem.eql(u8, token.string, CLOSE_EXP.string)) {
                            // Function call
                            try instructions.append(CALL_FUNCTION.call(Token{ .integer = @intCast(i32, fnArity) }));
                            tokensConsumed += 1;
                            break;
                        }

                        var expressionTokensConsumed = try compileExpression(tokens, instructions, argumentOffset);
                        fnArity += 1;
                        argumentOffset += expressionTokensConsumed;
                        tokensConsumed += expressionTokensConsumed;
                    }
                }
            } else if(std.mem.eql(u8, s, TIMES.string)) {
                var instructionsBefore = @intCast(i32, instructions.items.len);
                tokensConsumed += try compileExpression(tokens, instructions, offset + 1);

                try instructions.append(UNARY_NEGATIVE.call(null)); // Decrement TOS
                try instructions.append(LOAD_NAME.call(Token{ .string = "equal_zero" })); // Load EQUAL_ZERO function
                try instructions.append(COPY.call(Token{ .integer = 1 })); // Copy TOS
                try instructions.append(CALL_FUNCTION.call(Token{ .integer = 1 })); // Call EQUAL_ZERO
                try instructions.append(UNARY_NOT.call(null)); // Decrement TOS

                var instructionsAfter = @intCast(i32, instructions.items.len);

                try instructions.append(RELATIVE_JUMP_IF_TRUE.call(Token{ .integer = (instructionsBefore - instructionsAfter - 1) })); // Repeat 
            } else {
                // assume Token{ .string = "<name> "} is a variable
                try instructions.append(LOAD_NAME.call(tokens[offset]));
            }
        },
        .function => return CompilerError.NotImplemented,
        .boolean => return CompilerError.NotImplemented,
        .tuple => return CompilerError.NotImplemented,
        .instructions => return CompilerError.NotImplemented,
        .udf => return CompilerError.NotImplemented,
    }

    return tokensConsumed;
}

pub fn cleanupCompiledInstructions(allocator: std.mem.Allocator, code: []Instruction) void {
    for(code) |ins, idx|
        if(ins.arg) |arg|
            if(arg == Token.tuple) allocator.free(arg.tuple)
            else if(arg == Token.instructions) allocator.free(arg.instructions)
            else if(arg == Token.udf) code[idx].arg.?.udf.deinit();
    allocator.free(code);
}

// Testing helper
fn compileAndExpectByteCode(expected: []Instruction, program: []Token) !void {
    const allocator = std.testing.allocator;

    var actual = try compile(allocator, program);
    defer cleanupCompiledInstructions(allocator, actual);

    for(expected) |ins, idx| {
        std.testing.expect(idx < actual.len) catch |err| {
            std.debug.print("\n expected len = {}, actual len = {}\n", .{expected.len, actual.len});
            return err;
        };

        if(ins.arg != null and ins.arg.? == Token.tuple) {
            try std.testing.expectEqual(ins.opcode, actual[idx].opcode);
            try std.testing.expectEqualSlices(Token, ins.arg.?.tuple, actual[idx].arg.?.tuple);
        } else if(ins.arg != null and ins.arg.? == Token.instructions) {
            try std.testing.expectEqual(ins.opcode, actual[idx].opcode);
            try std.testing.expectEqualSlices(Instruction, ins.arg.?.instructions, actual[idx].arg.?.instructions);
        } else {
            std.testing.expect(std.meta.eql(ins, actual[idx])) catch |err| {
                std.debug.print("\n at {}:\n    expected = {},\n    actual   = {}\n", .{idx, ins, actual[idx]});
                return err;
            };
        }
    }
}

test "compile integer" {
    var value = Token{ .integer = 7 };
    var program = comptime parse(.{7});

    var expected = [1]Instruction{
        LOAD_CONST.call(value)
    };

    try compileAndExpectByteCode(&expected, program);
}

test "compile set variable" {
    const allocator = std.testing.allocator;

    var program = comptime parse(.{"val", "x", 5});
    var compiled = try compile(allocator, program);
    defer allocator.free(compiled);

    var expected = [2]Instruction{
        LOAD_CONST.call(program[2]),
        STORE_NAME.call(program[1])
    };

    try compileAndExpectByteCode(&expected, program);
}

test "compile get variable" {
    var program = comptime parse(.{"x"});

    var expected = [1]Instruction{
        LOAD_NAME.call(program[0]),
    };

    try compileAndExpectByteCode(&expected, program);
}

test "compile call function - 0 args" {
    var program = comptime parse(.{.{"hello"}}); // ["FN", "hello", "ENDFN"]

    var expected = [2]Instruction{
        LOAD_NAME.call(program[1]),
        CALL_FUNCTION.call(Token{.integer = 0}),
    };

    try compileAndExpectByteCode(&expected, program);
}

test "compile call function - 1 arg" {
    var program = comptime parse(.{.{"hello", 1}}); // ["FN", "hello", 1, "ENDFN"]

    var expected = [3]Instruction{
        LOAD_NAME.call(program[1]),
        LOAD_CONST.call(program[2]),
        CALL_FUNCTION.call(Token{.integer = 1}),
    };

    try compileAndExpectByteCode(&expected, program);
}

test "compile call function - n args" {
    var program = comptime parse(.{.{"hello", 1, 2, 3, 4}});

    var expected = [6]Instruction{
        LOAD_NAME.call(program[1]),
        LOAD_CONST.call(program[2]),
        LOAD_CONST.call(program[3]),
        LOAD_CONST.call(program[4]),
        LOAD_CONST.call(program[5]),
        CALL_FUNCTION.call(Token{.integer = 4}),
    };

    try compileAndExpectByteCode(&expected, program);
}

test "compile syntax error" {
    var program = comptime parse(.{"val", "x"});
    try std.testing.expectError(CompilerError.InvalidSyntax, compile(std.testing.allocator, program));
}

test "compile nesting" {
    var program = comptime parse(.{"val", "foo", .{"hello", 1, 2}});

    var expected = [5]Instruction{
        LOAD_NAME.call(program[3]),
        LOAD_CONST.call(program[4]),
        LOAD_CONST.call(program[5]),
        CALL_FUNCTION.call(Token{.integer = 2}),
        STORE_NAME.call(program[1]),
    };

    try compileAndExpectByteCode(&expected, program);
}

test "compile times loop" {
    var program = comptime parse(.{10, "times", .{"sum"}});

    var expected = [9]Instruction{
        // 10
        LOAD_CONST.call(Token{ .integer = 10 }),

        // .{"sum"}
        LOAD_NAME.call(Token{ .string = "sum" }),
        CALL_FUNCTION.call(Token{.integer = 0}),

        // "times"
        UNARY_NEGATIVE.call(null),
        LOAD_NAME.call(Token{ .string = "equal_zero" }),
        COPY.call(Token{ .integer = 1 }),
        CALL_FUNCTION.call(Token{.integer = 1}),
        UNARY_NOT.call(null),
        RELATIVE_JUMP_IF_TRUE.call(Token{.integer = -8}),
    };

    try compileAndExpectByteCode(&expected, program);
}

test "compile lambda" {
    var program = comptime parse(.{.{"lambda", .{"x"}, .{"sum", "x", 1}}});

    var expected_params = [1]Token{Token{.string = "x"}};
    var expected_instructions = [4]Instruction{
        LOAD_NAME.call(Token{ .string = "sum" }),
        LOAD_NAME.call(Token{ .string = "x" }),
        LOAD_CONST.call(Token{ .integer = 1 }),
        CALL_FUNCTION.call(Token{.integer = 2}),
    };

    var expected = [3]Instruction{
        LOAD_CONST.call(Token{ .tuple = &expected_params }),
        LOAD_CONST.call(Token{ .instructions = &expected_instructions }),
        MAKE_FUNCTION.call(Token{ .integer = 1 }),
    };

    try compileAndExpectByteCode(&expected, program);
}


test "compile assign lambda to variable" {
    var program = comptime parse(.{.{"val", "foo", .{"lambda", .{"x"}, .{"sum", "x", 1}}}, .{"foo", 2}});

    var expected_params = [1]Token{Token{.string = "x"}};
    var expected_instructions = [4]Instruction{
        LOAD_NAME.call(Token{ .string = "sum" }),
        LOAD_NAME.call(Token{ .string = "x" }),
        LOAD_CONST.call(Token{ .integer = 1 }),
        CALL_FUNCTION.call(Token{.integer = 2}),
    };

    var expected = [_]Instruction{
        LOAD_CONST.call(Token{ .tuple = &expected_params }),
        LOAD_CONST.call(Token{ .instructions = &expected_instructions }),
        MAKE_FUNCTION.call(Token{ .integer = 1 }),
        STORE_NAME.call(Token{ .string = "foo" }),
        LOAD_NAME.call(Token{ .string = "foo" }),
        LOAD_CONST.call(Token{ .integer = 2 }),
        CALL_FUNCTION.call(Token{.integer = 1 }),
    };

    try compileAndExpectByteCode(&expected, program);
}

test "compile if" {
    var program = comptime parse(.{.{"if", .{"equal_zero", 0}, 1, 2}});

    var expected = [_]Instruction{
        LOAD_NAME.call(Token{ .string = "equal_zero" }),
        LOAD_CONST.call(Token{ .integer = 0 }),
        CALL_FUNCTION.call(Token{ .integer = 1 }),
        RELATIVE_JUMP_IF_TRUE.call(Token{ .integer = 2 }),
        LOAD_CONST.call(Token{ .integer = 2 }),
        RELATIVE_JUMP.call(Token{ .integer = 1 }),
        LOAD_CONST.call(Token{ .integer = 1 }),
    };

    try compileAndExpectByteCode(&expected, program);
}