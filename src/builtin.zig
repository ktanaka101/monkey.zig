const std = @import("std");

const string = @import("string");

const object = @import("object.zig");
const EvaluatorError = @import("evaluator_error.zig").EvaluatorError;
const util = @import("evaluator_util.zig");

pub const TRUE = object.Boolean{ .value = true };
pub const FALSE = object.Boolean{ .value = false };
pub const NULL = object.Null{};

pub var TRUE_OBJECT = object.Object{ .boolean = TRUE };
pub var FALSE_OBJECT = object.Object{ .boolean = FALSE };
pub var NULL_OBJECT = object.Object{ .null_ = NULL };
pub var BUILTIN_FUNCTION_LEN_OBJECT = object.Object{
    .builtinFunction = object.BuiltinFunction{
        .function = BuiltinFunction.len,
    },
};
pub var BUILTIN_FUNCTION_FIRST_OBJECT = object.Object{
    .builtinFunction = object.BuiltinFunction{
        .function = BuiltinFunction.first,
    },
};
pub var BUILTIN_FUNCTION_LAST_OBJECT = object.Object{
    .builtinFunction = object.BuiltinFunction{
        .function = BuiltinFunction.last,
    },
};
pub var BUILTIN_FUNCTION_REST_OBJECT = object.Object{
    .builtinFunction = object.BuiltinFunction{
        .function = BuiltinFunction.rest,
    },
};
pub var BUILTIN_FUNCTION_PUSH_OBJECT = object.Object{
    .builtinFunction = object.BuiltinFunction{
        .function = BuiltinFunction.push,
    },
};
pub var BUILTIN_FUNCTION_PUTS_OBJECT = object.Object{
    .builtinFunction = object.BuiltinFunction{
        .function = BuiltinFunction.puts,
    },
};

pub const BuiltinFunction = enum {
    len,
    first,
    last,
    rest,
    push,
    puts,

    pub fn call(self: BuiltinFunction, allocator: std.mem.Allocator, args: std.ArrayList(*object.Object)) EvaluatorError!*object.Object {
        return switch (self) {
            .len => try len(allocator, args),
            .first => try first(allocator, args),
            .last => try last(allocator, args),
            .rest => try rest(allocator, args),
            .push => try push(allocator, args),
            .puts => try puts(allocator, args),
        };
    }
};

fn len(allocator: std.mem.Allocator, args: std.ArrayList(*object.Object)) !*object.Object {
    if (try expectArgumentNumber(allocator, 1, args)) |err| {
        return err;
    }

    switch (args.items[0].*) {
        .string => |argString| {
            return util.newInteger(allocator, @intCast(argString.value.len));
        },
        .array => |argArray| return util.newInteger(allocator, @intCast(argArray.items.items.len)),
        else => return util.newError(
            allocator,
            "argument to 'len' not supported, got {s}",
            .{args.items[0].typeName()},
        ),
    }
}

fn first(allocator: std.mem.Allocator, args: std.ArrayList(*object.Object)) !*object.Object {
    if (try expectArgumentNumber(allocator, 1, args)) |err| {
        return err;
    }

    switch (args.items[0].*) {
        .array => |argArray| {
            if (argArray.items.items.len == 0) {
                return &NULL_OBJECT;
            }

            return argArray.items.items[0];
        },
        else => return util.newError(
            allocator,
            "argument to 'first' must be ARRAY, got {s}",
            .{args.items[0].typeName()},
        ),
    }
}

fn last(allocator: std.mem.Allocator, args: std.ArrayList(*object.Object)) !*object.Object {
    if (try expectArgumentNumber(allocator, 1, args)) |err| {
        return err;
    }

    switch (args.items[0].*) {
        .array => |argArray| {
            if (argArray.items.items.len == 0) {
                return &NULL_OBJECT;
            }

            return argArray.items.items[argArray.items.items.len - 1];
        },
        else => return util.newError(
            allocator,
            "argument to 'last' must be ARRAY, got {s}",
            .{args.items[0].typeName()},
        ),
    }
}

fn rest(allocator: std.mem.Allocator, args: std.ArrayList(*object.Object)) !*object.Object {
    if (try expectArgumentNumber(allocator, 1, args)) |err| {
        return err;
    }

    switch (args.items[0].*) {
        .array => |argArray| {
            if (argArray.items.items.len == 0) {
                return &NULL_OBJECT;
            }

            var newItems = std.ArrayList(*object.Object).init(allocator);
            var i: usize = 1;
            while (i < argArray.items.items.len) : (i += 1) {
                newItems.append(argArray.items.items[i]) catch return EvaluatorError.MemoryAllocation;
            }

            return util.newArray(allocator, newItems);
        },
        else => return util.newError(
            allocator,
            "argument to 'rest' must be ARRAY, got {s}",
            .{args.items[0].typeName()},
        ),
    }
}

fn push(allocator: std.mem.Allocator, args: std.ArrayList(*object.Object)) !*object.Object {
    if (try expectArgumentNumber(allocator, 2, args)) |err| {
        return err;
    }

    switch (args.items[0].*) {
        .array => |argArray| {
            var newItems = std.ArrayList(*object.Object).init(allocator);
            var i: usize = 0;
            while (i < argArray.items.items.len) : (i += 1) {
                newItems.append(argArray.items.items[i]) catch return EvaluatorError.MemoryAllocation;
            }
            newItems.append(args.items[1]) catch return EvaluatorError.MemoryAllocation;

            return util.newArray(allocator, newItems);
        },
        else => return util.newError(
            allocator,
            "argument to 'push' must be ARRAY, got {s}",
            .{args.items[0].typeName()},
        ),
    }
}

fn puts(allocator: std.mem.Allocator, args: std.ArrayList(*object.Object)) !*object.Object {
    var buffer = string.String.init(allocator);
    var i: usize = 0;
    while (i < args.items.len) : (i += 1) {
        args.items[i].toString(&buffer) catch return EvaluatorError.MemoryAllocation;
    }
    std.debug.print("{s}\n", .{buffer.str()});

    return &NULL_OBJECT;
}

fn expectArgumentNumber(allocator: std.mem.Allocator, expect: usize, args: std.ArrayList(*object.Object)) !?*object.Object {
    if (args.items.len != expect) {
        return try util.newError(
            allocator,
            "wrong number of arguments. got={}, want={}",
            .{ args.items.len, expect },
        );
    } else {
        return null;
    }
}
