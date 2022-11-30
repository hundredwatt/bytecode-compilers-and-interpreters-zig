const std = @import("std");

const Primitives = @import("primitives.zig");
const Token = Primitives.Token;

// Reserved Words
pub const OPEN_EXP = Token{ .string = "(" }; 
pub const CLOSE_EXP = Token{ .string = ")" }; 
pub const VAL = Token{ .string = "val" }; 
pub const IF = Token{ .string = "if" }; 
pub const TIMES = Token{ .string = "times" }; 
pub const LAMBDA = Token{ .string = "lambda" }; 

pub fn parse(values: anytype) []Token {
    var len = outputFieldLen(values, true);

    var output: [len]Token = undefined;

    toTokens(values, &output, 0, true);

    return &output;
}

fn toTokens(value: anytype, output: []Token, offset: usize, root: bool) void {
    return switch(@typeInfo(@TypeOf(value))) {
        .Struct => {
            var structOffset = offset;
            var isExp = false;
            for (std.meta.fields(@TypeOf(value))) |field, idx| {
                const field_value = @field(value, field.name); 

                if(!root and idx == 0 and !std.mem.eql(u8, VAL.string, field_value)) {
                    isExp = true;
                    output[structOffset] = OPEN_EXP;
                    structOffset += 1;
                }

                toTokens(field_value, output, structOffset, false);
                structOffset += outputFieldLen(field_value, false);
            }

            if(isExp) {
                output[structOffset] = CLOSE_EXP;
                structOffset += 1;
            }
        },
        .ComptimeInt => output[offset] = Token{ .integer = value },
        .Pointer => output[offset] = Token { .string = value },
        else => @compileError("unexpected type: " ++ @typeName(value)),
    };
}

fn outputFieldLen(value: anytype, root: bool) u32 {
    return switch(@typeInfo(@TypeOf(value))) {
        .Struct => {
            var acc: u32 = 0;
            for (std.meta.fields(@TypeOf(value))) |field, idx| {
                const field_value = @field(value, field.name); 

                // Add room for FN/ENDFN keywords
                if(!root and idx == 0 and !std.mem.eql(u8, VAL.string, field_value)) acc += 2;

                acc += outputFieldLen(field_value, false);
            }
            return acc;
        },
        .ComptimeInt => 1,
        .Pointer => 1,
        else => @compileError("unexpected type: " ++ @typeName(value)),
    };
}

test "parse numbers and strings" {
    var program = comptime parse(.{"foo", 5});

    var expected = [_]Token{
        Token{ .string = "foo" },
        Token{ .integer = 5},
    };

    try std.testing.expectEqualSlices(Token, &expected, program);
}

test "set variable expressions" {
    var program = comptime parse(.{.{"val", "foo", 5}, "foo"});

    var expected = [4]Token{
        Token{ .string = "val" },
        Token{ .string = "foo" },
        Token{ .integer = 5},
        Token{ .string = "foo" },
    };

    try std.testing.expectEqualSlices(Token, &expected, program);
}

test "parse functions" {
    var program = comptime parse(.{"foo", 5, .{"hello", 1}});

    var expected = [_]Token{
        Token{ .string = "foo" },
        Token{ .integer = 5},
        OPEN_EXP,
        Token{ .string = "hello" },
        Token{ .integer = 1},
        CLOSE_EXP,
    };

    try std.testing.expectEqualSlices(Token, &expected, program);
}

test "parse lambda" {
    var program = comptime parse(.{.{"lambda", .{"x"}, .{"sum", "x", 1}}});

    var expected = [_]Token{
        OPEN_EXP,
        LAMBDA,
        OPEN_EXP,
        Token{ .string = "x"},
        CLOSE_EXP,
        OPEN_EXP,
        Token{ .string = "sum" },
        Token{ .string = "x" },
        Token{ .integer = 1},
        CLOSE_EXP,
        CLOSE_EXP,
    };

    try std.testing.expectEqualSlices(Token, &expected, program); 
}

