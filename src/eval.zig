const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const Primitives = @import("primitives.zig");
const Opcode = Primitives.Opcode;
const Token = Primitives.Token;
const Instruction = Primitives.Instruction;
const Env = Primitives.Env;
const UDF = Primitives.UDF;

const Parser = @import("parser.zig");
const parse = Parser.parse;

const Compiler = @import("compiler.zig");
const compile = Compiler.compile;
const cleanupCompiledInstructions = Compiler.cleanupCompiledInstructions;

const RuntimeError = error {
    OutOfMemory,
    ReferenceError,
    TypeError,
    ArityError,
    MissingArgument,
};

pub fn eval(allocator: Allocator, code: []Instruction, env: *Env) RuntimeError!?Token {
    var pc: usize = 0;
    var stack = ArrayList(Token).init(allocator);
    defer stack.deinit();

    while(pc < code.len) : (pc += 1) {
        var ins = code[pc];
        var op = ins.opcode;

        var arg: Token = switch(op) {
            Opcode.UNARY_NEGATIVE => undefined,
            Opcode.UNARY_NOT => undefined,
            else => ins.arg orelse return RuntimeError.MissingArgument,
        };

        switch(op) {
            Opcode.LOAD_CONST => try stack.append(arg),
            Opcode.STORE_NAME => try env.put(arg.string, stack.pop()),
            Opcode.LOAD_NAME => try stack.append(try env.lookup(arg.string)),
            Opcode.COPY => try stack.append(stack.items[stack.items.len - 1 - @intCast(usize, arg.integer)]),
            Opcode.UNARY_NEGATIVE => {
                var tos = stack.pop();
                if(tos != Token.integer) return RuntimeError.TypeError;

                try stack.append(Token{.integer = (tos.integer - 1)});
            },
            Opcode.UNARY_NOT => {
                var tos = stack.pop();
                if(tos != Token.boolean) return RuntimeError.TypeError;

                try stack.append(Token{.boolean = !tos.boolean});
            },
            Opcode.RELATIVE_JUMP => {
                pc = @intCast(usize, @intCast(i32, pc) + arg.integer);
            },
            Opcode.RELATIVE_JUMP_IF_TRUE => {
                var tos = stack.pop();
                if(tos != Token.boolean) return RuntimeError.TypeError;

                if(tos.boolean) pc = @intCast(usize, @intCast(i32, pc) + arg.integer);
            },
            Opcode.MAKE_FUNCTION => {
                var arity: usize = @intCast(usize, arg.integer);
                var body = stack.pop();
                var params = stack.pop();
                if(params.tuple.len != arity) return RuntimeError.TypeError;

                var function = try UDF.init(allocator, params.tuple, body.instructions, env);

                try stack.append(Token{.udf = function});

            },
            Opcode.CALL_FUNCTION => {
                var arity: usize = @intCast(usize, arg.integer);
                var args = try allocator.alloc(Token, arity);
                defer allocator.free(args);

                if(arity > 0) {
                    var position = arity - 1;
                    while(true) {
                        args[position] = stack.pop();

                        if(position == 0) break;
                        position -= 1;
                    }
                }

                var function_token = stack.pop();
                switch(function_token) {
                    .function => {
                        if(try function_token.function.call(args)) |result| 
                            try stack.append(result); 
                    },
                    .udf => {
                        var udf = function_token.udf;
                        if(args.len != udf.params.len) return RuntimeError.TypeError;
                        var udf_env = Env.init(allocator, env);
                        defer udf_env.deinit();
                        for(udf.params) |param, idx| {
                            try udf_env.put(param.string, args[idx]);
                        }

                        if(try eval(allocator, udf.body, &udf_env)) |result|
                            try stack.append(result); 
                    },
                    else => return RuntimeError.TypeError,
                }
            },
        }
    }

    return stack.popOrNull();
}

// Convenience method for executing a program represent as a []Token
pub fn run(allocator: std.mem.Allocator, program: []Token) anyerror!?Token {
    var compiled: []Instruction = try compile(allocator, program);
    defer cleanupCompiledInstructions(allocator, compiled);

    var env = Env.init(allocator, null);
    defer env.deinit();

    return try eval(allocator, compiled, &env);
}


test "eval integer" {
    var program = comptime parse(.{5});

    try std.testing.expect(std.meta.eql(try run(std.testing.allocator, program), Token{ .integer = 5 }));
}

test "eval set variable" {
    const allocator = std.testing.allocator;

    var program = comptime parse(.{"val", "x", 5});
    var compiled: []Instruction = try compile(allocator, program);
    defer cleanupCompiledInstructions(allocator, compiled);

    var env = Env.init(allocator, null);
    defer env.deinit();

    _ = try eval(allocator, compiled, &env);

    try std.testing.expectEqual(program[2].integer, (try env.lookup("x")).integer);
}

test "eval get variable" {
    const allocator = std.testing.allocator;

    var value = Token{ .integer = 7 };
    var program = comptime parse(.{"x"});
    var compiled: []Instruction = try compile(allocator, program);
    defer allocator.free(compiled);

    var env = Env.init(allocator, null);
    defer env.deinit();

    try env.put("x", value);

    try std.testing.expectEqual(value.integer, (try eval(allocator, compiled, &env)).?.integer);
}

test "eval function" {
    var result: ?Token = undefined;

    result = try run(std.testing.allocator, comptime parse(.{.{"sum"}}));
    try std.testing.expectEqual(@as(i32, 0), result.?.integer);

    result = try run(std.testing.allocator, comptime parse(.{.{"sum", 1}}));
    try std.testing.expectEqual(@as(i32, 1), result.?.integer);

    result = try run(std.testing.allocator, comptime parse(.{.{"sum", 1, 2, 3}}));
    try std.testing.expectEqual(@as(i32, 6), result.?.integer);
}

test "eval nesting" {
    var result = try run(std.testing.allocator, 
        comptime parse(.{.{"val", "foo", .{"sum", 1, 2, 3}}, "foo"}));
    try std.testing.expectEqual(@as(i32, 6), result.?.integer);

    result = try run(std.testing.allocator, 
        comptime parse(.{"val", "foo", .{"sum", 2, 3, 4}, "foo"}));
    try std.testing.expectEqual(@as(i32, 9), result.?.integer);

    result = try run(std.testing.allocator, 
        comptime parse(.{"val", "foo", .{"sum", 5, .{"sum", 6, 7}, 8}, "foo"}));
    try std.testing.expectEqual(@as(i32, 26), result.?.integer);

    result = try run(std.testing.allocator, 
        comptime parse(.{"val", "foo", 11, "val", "bar", .{"sum", 12, 13}, .{"sum", "foo", "bar"}}));
    try std.testing.expectEqual(@as(i32, 36), result.?.integer);
}

test "eval times" {
    var result = try run(std.testing.allocator, 
        comptime parse(.{
            .{"val", "foo", 1}, 
            10,
            "times",
            .{"val", "foo", .{"sum", "foo", 3}},
            "foo"}));
    try std.testing.expectEqual(@as(i32, 31), result.?.integer);
}

test "eval lambda" {
    var result = try run(std.testing.allocator, 
        comptime parse(.{
            .{"val", "foo", 
                .{"lambda", .{"x"}, .{"sum", "x", 1}}}, 
            .{"foo", 2}}
        ));
    try std.testing.expectEqual(@as(i32, 3), result.?.integer);
}

test "eval if" {
    var result = try run(std.testing.allocator, 
        comptime parse(.{
            .{"val", "fib", 
                .{"lambda", 
                    .{"n"}, 
                    .{"if", .{"lteq", "n", 1},
                    "n",
                    .{"sum", 
                        .{"fib", .{"sum", "n", -1}},
                        .{"fib", .{"sum", "n", -2}}}}}}, 
            .{"fib", 7}}
        ));
    try std.testing.expectEqual(@as(i32, 13), result.?.integer);
}

test "eval if 2" {
    var result = try run(std.testing.allocator, 
        comptime parse(.{
            .{"val", "factorial", 
                .{"lambda", 
                    .{"n"}, 
                    .{"if", .{"equal_zero", "n"},
                    1,
                    .{"product", 
                        "n",
                        .{"factorial", .{"sum", "n", -1}}}}}}, 
            .{"factorial", 5}}
        ));
    try std.testing.expectEqual(@as(i32, 120), result.?.integer);
}
