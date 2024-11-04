comptime {
    std.testing.refAllDecls(@This());
}

const std = @import("std");

const String = @import("string").String;

const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Evaluator = @import("evaluator.zig").Evaluator;
const Environment = @import("environment.zig").Environment;

pub fn main() anyerror!void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var rootAllocator = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer rootAllocator.deinit();
    const allocator = rootAllocator.allocator();

    var env = Environment.new(allocator);
    var evaluator = Evaluator.new(allocator);

    // store input strings
    var inputBuf: [65536]u8 = undefined;
    var lastPos: usize = 0;

    while (true) {
        try stdout.print(">> ", .{});

        if (try stdin.readUntilDelimiterOrEof(inputBuf[lastPos..], '\n')) |input| {
            lastPos += input.len;

            var lexer = Lexer.new(input);
            var parser = Parser.new(&lexer, allocator);
            var program = parser.parseProgram() catch |err| {
                try stdout.print("Parsing error: {}\n", .{err});
                continue;
            };

            const object = evaluator.evalProgram(&program, &env) catch |err| {
                try stdout.print("Evaluating error: {}\n", .{err});
                continue;
            };
            switch (object.*) {
                .error_ => |error_| {
                    try stdout.print("Error: {s}\n", .{error_.message});
                },
                else => {
                    var objPrintBuf = String.init(allocator);
                    try object.toString(&objPrintBuf);
                    try stdout.print("{s}\n", .{objPrintBuf.str()});
                },
            }
        } else {
            break;
        }
    }
}
