const std = @import("std");

const parse = @import("parser.zig").parse;
const run = @import("eval.zig").run;

pub fn main() anyerror!void {
    const allocator = std.heap.page_allocator;

    var program = comptime parse(.{
        .{"val", "factorial", 
            .{"lambda", 
                .{"n"}, 
                .{"if", .{"equal_zero", "n"},
                1,
                .{"product", 
                        "n",
                        .{"factorial", .{"sum", "n", -1}}}}}}, 
        .{"val", "y", .{"sum", 2, 3}}, 
        .{"factorial", "y"}
    });
    var result = try run(allocator, program);

    std.log.info("RESULT: {?}", .{result}); // RESULT: Token{ .integer = 120 }
}