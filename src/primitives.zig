const std = @import("std");

pub const LOAD_CONST = Instruction.init(Opcode.LOAD_CONST, Token{ .integer = 0 });
pub const STORE_NAME = Instruction.init(Opcode.STORE_NAME, Token{ .string = "name" });
pub const LOAD_NAME  = Instruction.init(Opcode.LOAD_NAME, Token{ .string = "name" });
pub const CALL_FUNCTION = Instruction.init(Opcode.CALL_FUNCTION, Token{ .integer = 0});
pub const MAKE_FUNCTION = Instruction.init(Opcode.MAKE_FUNCTION, Token{ .integer = 0});
pub const UNARY_NEGATIVE = Instruction.init(Opcode.UNARY_NEGATIVE, null);
pub const UNARY_NOT = Instruction.init(Opcode.UNARY_NOT, null);
pub const RELATIVE_JUMP = Instruction.init(Opcode.RELATIVE_JUMP, Token{ .integer = 0});
pub const RELATIVE_JUMP_IF_TRUE = Instruction.init(Opcode.RELATIVE_JUMP_IF_TRUE, Token{ .integer = 0});
pub const COPY = Instruction.init(Opcode.COPY, Token{ .integer = 0});

pub const PrimitiveError = error {
    OutOfMemory,
    ReferenceError,
    TypeError,
    ArityError,
};

pub const Opcode = enum(u8) {
    LOAD_CONST,
    STORE_NAME,
    LOAD_NAME,
    CALL_FUNCTION,
    MAKE_FUNCTION,
    UNARY_NEGATIVE,
    UNARY_NOT,
    RELATIVE_JUMP,
    RELATIVE_JUMP_IF_TRUE,
    COPY,
};

// Builtin Function
pub const Function = enum(u8) {
    SUM,
    PRODUCT,
    EQUAL_ZERO,
    LTEQ,

    pub fn call(self: Function, args: []Token) PrimitiveError!?Token {
        switch(self) {
            Function.SUM => {
                var acc: i32 = 0;
                for(args) |arg| {
                    if(arg != Token.integer) return PrimitiveError.TypeError;

                    acc += arg.integer;
                }
                return Token{ .integer = acc };
            },
            Function.PRODUCT => {
                var acc: i32 = 1;
                for(args) |arg| {
                    if(arg != Token.integer) return PrimitiveError.TypeError;

                    acc *= arg.integer;
                }
                return Token{ .integer = acc };
            },
            Function.EQUAL_ZERO => {
                if(args.len != 1) return PrimitiveError.ArityError;
                var arg = args[0];
                if(arg != Token.integer) return PrimitiveError.TypeError;

                return Token{ .boolean = (arg.integer == 0) };
            },
            Function.LTEQ => {
                if(args.len != 2) return PrimitiveError.ArityError;
                var lharg = args[0];
                var rharg = args[1];
                if(lharg != Token.integer) return PrimitiveError.TypeError;
                if(rharg != Token.integer) return PrimitiveError.TypeError;

                return Token{ .boolean = (lharg.integer <= rharg.integer) };
            },
        }

        return null;
    }
};

const Functions = std.enums.values(Function);

pub const Token = union(enum) {
    integer: i32,
    string: []const u8,
    function: Function,
    boolean: bool,
    tuple: []Token,
    instructions: []Instruction,
    udf: UDF,
};

pub const Instruction = struct {
    opcode: Opcode,
    arg: ?Token,

    pub fn init(opcode: Opcode, arg: ?Token) Instruction {
        return Instruction{ .opcode = opcode, .arg = arg};
    }

    pub fn call(self: *Instruction, arg: ?Token) Instruction {
        return init(self.opcode, arg);
    }
};

pub const Env = struct {
    table: std.StringHashMap(Token),
    parent: ?*@This() = null,
    // TODO: support for nesting

    pub fn init(allocator: std.mem.Allocator, parent: ?*Env) Env {
        return Env{ .table = std.StringHashMap(Token).init(allocator), .parent = parent };
    }

    pub fn deinit(self: *Env) void {
        self.table.deinit();
    }

    pub fn put(self: *Env, name: []const u8, value: Token) PrimitiveError!void {
        try self.table.put(name, value);
    }

    pub fn lookup(self: *Env, name: []const u8) PrimitiveError!Token {
        // Builtin function names take precedence over everything else
        if(try self.lookupFunction(name)) |function| { 
            return Token{ .function = function }; 
        }

        return (try self.resolve(name)).table.get(name) orelse PrimitiveError.ReferenceError;
    }

    fn resolve(self: *Env, name: []const u8) PrimitiveError!*Env {
        if(self.table.getKey(name) != null) return self;

        if(self.parent == null) return PrimitiveError.ReferenceError;

        return self.parent.?.resolve(name);
    }

    fn lookupFunction(self: *Env, name: []const u8) PrimitiveError!?Function {
        for(Functions) |function| {
            var upper_name = try std.ascii.allocUpperString(self.table.allocator, name);
            defer self.table.allocator.free(upper_name);

            if(std.mem.eql(u8, @tagName(function), upper_name)) return function;
        }

        return null;
    }
};

// User-defined function
pub const UDF = struct {
    allocator: std.mem.Allocator,
    params: []Token,
    body: []Instruction,
    env: *Env,

    pub fn init(allocator: std.mem.Allocator, params: []Token, body: []Instruction, env: *Env) std.mem.Allocator.Error!UDF {
        return UDF{ .allocator = allocator, .params = params, .body = body, .env = env };
    }

    pub fn deinit(self: *UDF) void {
        self.allocator.free(self.params);
        self.allocator.free(self.body);
    }
};

test "Env" {
    var parent = Env.init(std.testing.allocator, null);
    try parent.put("foo", Token{.integer = 1});
    defer parent.deinit();

    try std.testing.expect(std.meta.eql(Token{.integer = 1}, try parent.lookup("foo")));
    try std.testing.expectError(PrimitiveError.ReferenceError, parent.lookup("bar"));

    var child = Env.init(std.testing.allocator, &parent);
    try child.put("bar", Token{.integer = 2});
    defer child.deinit();

    try std.testing.expect(std.meta.eql(Token{.integer = 1}, try child.lookup("foo")));
    try std.testing.expect(std.meta.eql(Token{.integer = 2}, try child.lookup("bar")));
    try std.testing.expectError(PrimitiveError.ReferenceError, parent.lookup("baz"));
}