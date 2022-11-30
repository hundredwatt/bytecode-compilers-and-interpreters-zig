const std = @import("std");
const ArrayList = std.ArrayList;

const Primitives = @import("primitives.zig");
const Env = Primitives.Env;
const Token = Primitives.Token;
const Instruction = Primitives.Instruction;
const Opcode = Primitives.Opcode;
const UDF = Primitives.UDF;

var LOAD_CONST = Primitives.LOAD_CONST;
var STORE_NAME = Primitives.STORE_NAME;
var LOAD_NAME = Primitives.LOAD_NAME;

const parse = @import("parser.zig").parse;

const RuntimeError = error {
    TypeError,
    NotImplemented,
};

pub fn native_eval(allocator: std.mem.Allocator, comptime code: []Instruction, env: *Env) anyerror!?Token {
    comptime var pc: usize = 0;
    var stack = ArrayList(Token).init(allocator);
    defer stack.deinit();

    inline while(pc < code.len) : (pc += 1) {
        comptime var ins = code[pc];
        comptime var op = ins.opcode;

        comptime var arg = ins.arg orelse @compileError("missing arg");

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
                    // .udf => {
                    //     var udf = function_token.udf;
                    //     if(args.len != udf.params.len) return RuntimeError.TypeError;
                    //     var udf_env = Env.init(allocator, env);
                    //     defer udf_env.deinit();
                    //     for(udf.params) |param, idx| {
                    //         try udf_env.put(param.string, args[idx]);
                    //     }

                    //     if(try native_eval(allocator, udf.body, &udf_env)) |result|
                    //         try stack.append(result); 
                    // },
                    else => return RuntimeError.TypeError,
                }
            },
        }
    }

    return stack.popOrNull();
}

test "sum1 native" {
    comptime var compiled: [4]Instruction = undefined;
    compiled[0].opcode = Opcode.LOAD_NAME;
    compiled[0].arg = Token{ .string = "sum" };
    compiled[1].opcode = Opcode.LOAD_NAME;
    compiled[1].arg = Token{ .string = "x" };
    compiled[2].opcode = Opcode.LOAD_CONST;
    compiled[2].arg = Token{ .integer = 1 };
    compiled[3].opcode = Opcode.CALL_FUNCTION;
    compiled[3].arg = Token{ .integer = 2 };

    var env = Env.init(std.testing.allocator, null);
    defer env.deinit();

    try env.put("x", Token{ .integer = 11 });

    var result = try native_eval(std.testing.allocator, &compiled, &env);

    try std.testing.expectEqual(Token{ .integer = 12 }, result.?);
}

test "sum1 native - with args" {
    comptime var compiled: [4]Instruction = undefined;
    compiled[0].opcode = Opcode.LOAD_NAME;
    compiled[0].arg = Token{ .string = "sum" };
    compiled[1].opcode = Opcode.LOAD_NAME;
    compiled[1].arg = Token{ .string = "x" };
    compiled[2].opcode = Opcode.LOAD_CONST;
    compiled[2].arg = Token{ .integer = 1 };
    compiled[3].opcode = Opcode.CALL_FUNCTION;
    compiled[3].arg = Token{ .integer = 2 };

    var env = Env.init(std.testing.allocator, null);
    defer env.deinit();

    try env.put("x", Token{ .integer = 11 });

    var result = try native_eval(std.testing.allocator, &compiled, &env);

    try std.testing.expectEqual(Token{ .integer = 12 }, result.?);
}