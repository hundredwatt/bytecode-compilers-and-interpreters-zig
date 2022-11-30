const std = @import("std");

const Primitives = @import("primitives.zig");
const Opcode = Primitives.Opcode;
const Token = Primitives.Token;
const Instruction = Primitives.Instruction;
const Env = Primitives.Env;
const UDF = Primitives.UDF;

var LOAD_CONST = Primitives.LOAD_CONST;
var STORE_NAME = Primitives.STORE_NAME;
var LOAD_NAME = Primitives.LOAD_NAME;
var CALL_FUNCTION = Primitives.CALL_FUNCTION;

const parse = @import("parser.zig").parse;
const run = @import("eval.zig").run;
const eval = @import("eval.zig").eval;

const Compiler = @import("compiler.zig");
const compile = Compiler.compile;
const cleanupCompiledInstructions = Compiler.cleanupCompiledInstructions;

fn native_compile(name: []const u8, udf: *UDF) ![]const u8 {
    const allocator = std.heap.page_allocator;

    const zig_exe = std.os.getenv("_").?;
    var buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const build_root = try std.os.getcwd(&buf);
    const cache_root = "zig-cache";
    const global_cache_root = "/Users/jason/.cache/zig";

    const b = try std.build.Builder.create(allocator, zig_exe, build_root, cache_root, global_cache_root);
    b.verbose = true;
    defer b.destroy();

    const mode = b.standardReleaseOptions();

    // const primitives = b.addStaticLibrary("primitives", "src/primitives.zig");
    // primitives.setBuildMode(mode);
    // primitives.install();

    // TODO
    _ = udf;
    const file = b.addWriteFile("native_library.zig", 
        \\ const Primitives = @import("primitives");
        \\ const native_eval = @import("native_eval").native_eval; 
        \\ const Opcode = Primitives.Opcode;
        \\ const Token = Primitives.Token;
        \\ const Instruction = Primitives.Instruction;
        \\ const Env = Primitives.Env;
        \\
        \\ const allocator = @import("std").heap.page_allocator;
        \\
        \\ export fn sum1_shortcut(x: i32) i32 { return x + 1; }
        \\
        \\ export fn sum1(x: i32) i32 { 
        \\   comptime var compiled: [4]Instruction = undefined;
        \\   compiled[0].opcode = Opcode.LOAD_NAME;
        \\   compiled[0].arg = Token{ .string = "sum" };
        \\   compiled[1].opcode = Opcode.LOAD_NAME;
        \\   compiled[1].arg = Token{ .string = "x" };
        \\   compiled[2].opcode = Opcode.LOAD_CONST;
        \\   compiled[2].arg = Token{ .integer = 1 };
        \\   compiled[3].opcode = Opcode.CALL_FUNCTION;
        \\   compiled[3].arg = Token{ .integer = 2 }; 
        \\
        \\   var env = Env.init(allocator, null);
        \\   defer env.deinit();
        \\   env.put("x", Token{ .integer = x }) catch { @panic("out of memory"); };
        \\   
        \\   var result = native_eval(allocator, &compiled, &env) catch { @panic("eval failed"); };
        \\   
        \\   return result.?.integer;
        \\ } 
    );

    const lib = b.addSharedLibrarySource(name, file.getFileSource("native_library.zig").?, b.version(0, 0, 1));
    lib.addPackagePath("primitives", "src/primitives.zig");
    lib.addPackagePath("native_eval", "src/native_eval.zig");
    lib.setBuildMode(mode);
    lib.install();

    var install_prefix: ?[]const u8 = cache_root ++ "/jit-cache";
    var dir_list = std.build.Builder.DirList{};
    b.resolveInstallPrefix(install_prefix, dir_list);

    var step_names = try allocator.alloc([]const u8, 0);
    try b.make(step_names);

    var artifact = std.ArrayList(u8).init(allocator);
    for(install_prefix.?) |l| try artifact.append(l);
    for("/lib/lib") |l| try artifact.append(l);
    for(name) |l| try artifact.append(l);
    for(".dylib") |l| try artifact.append(l);

    defer artifact.deinit();

    return artifact.toOwnedSlice();
}

const ret_type = fn (i32) callconv(.C) i32;

fn native_load(lib_path: []const u8, symbol: [:0]const u8) !ret_type {
    var lib = try std.DynLib.open(lib_path);
    defer lib.close();

    const func = lib.lookup(ret_type, symbol) orelse return error.SymbolNotFound;

    return func;
}

test "sum1" {
    var params = [_]Token{Token{ .string = "x" }};
    var body = [_]Instruction{
        LOAD_NAME.call(Token{ .string = "sum" }),
        LOAD_NAME.call(Token{ .string = "x" }),
        LOAD_CONST.call(Token{ .integer = 1 }),
        CALL_FUNCTION.call(Token{ .integer = 2 }),
    };
    var env = Env.init(std.testing.allocator, null);
    defer env.deinit();

    var sum1_udf = try UDF.init(std.testing.allocator, &params, &body, &env);

    var lib = try native_compile("sum1", &sum1_udf);
    var sum1_native = try native_load(lib, "sum1");

    try std.testing.expectEqual(@as(i32, 22), sum1_native(21));
    try std.testing.expectEqual(@as(i32, 78), sum1_native(77));
}

pub fn sum1(x: i32) i32 { return x + 1; }

const Timer = std.time.Timer;

pub fn main() !void {
    var params = [_]Token{Token{ .string = "x" }};
    var body = [_]Instruction{
        LOAD_NAME.call(Token{ .string = "sum" }),
        LOAD_NAME.call(Token{ .string = "x" }),
        LOAD_CONST.call(Token{ .integer = 1 }),
        CALL_FUNCTION.call(Token{ .integer = 2 }),
    };
    var env = Env.init(std.testing.allocator, null);
    defer env.deinit();

    var sum1_udf = try UDF.init(std.testing.allocator, &params, &body, &env);

    var lib = try native_compile("sum1", &sum1_udf);
    var sum1_native = try native_load(lib, "sum1");
    var sum1_native_shortcut = try native_load(lib, "sum1_shortcut");
    try sum1_udf.env.put("x", Token{.integer = 12313});

    var trials: u64 = 10000;

    var i: i32 = 0;
    var timer = Timer.start() catch unreachable;
    while(i < trials) : (i += 1) {
        _ = try eval(std.testing.allocator, sum1_udf.body, sum1_udf.env);
    }
    var dur = timer.read();
    std.log.info("sum1_udf: {} ms\t{} ops/s\n", .{dur / @as(u64, 1e6), (@as(u64, trials) * @as(u64, 1e9) / dur)}); 

    trials = 200000;

    i = 0;
    timer.reset();
    while(i < trials) : (i += 1) {
        _ = sum1_native(i);
    }
    dur = timer.read();
    std.log.info("sum1_native: {} ms\t{} ops/s\n", .{dur / @as(u64, 1e6), (@as(u64, trials) * @as(u64, 1e9) / dur)}); 

    trials = 1000000;

    i = 0;
    timer.reset();
    while(i < trials) : (i += 1) {
        _ = sum1_native_shortcut(i);
    }
    dur = timer.read();
    std.log.info("sum1_native_shortcut: {} ms\t{} ops/s\n", .{dur / @as(u64, 1e6), (@as(u64, trials) * @as(u64, 1e9) / dur)}); 

    i = 0;
    timer.reset();
    while(i < trials) : (i += 1) {
        _ = sum1(i);
    }
    dur = timer.read();
    std.log.info("sum1: {} ms\t{} ops/s\n", .{dur / @as(u64, 1e6), (@as(u64, trials) * @as(u64, 1e9) / dur)}); 

    i = 0;
    timer.reset();
    while(i < trials) : (i += 1) {
        _ = i;
    }
    dur = timer.read();
    std.log.info("noop: {} ms\t{d} ops/s\n", .{dur / @as(u64, 1e6), (@as(u64, trials) * @as(u64, 1e9) / dur)}); 
}