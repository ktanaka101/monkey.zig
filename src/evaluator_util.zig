const std = @import("std");

const ast = @import("ast.zig");
const object = @import("object.zig");
const Environment = @import("environment.zig").Environment;
const EvaluatorError = @import("evaluator_error.zig").EvaluatorError;

pub fn newError(allocator: std.mem.Allocator, comptime fmt: []const u8, args: anytype) !*object.Object {
    const message = std.fmt.allocPrint(allocator, fmt, args) catch return EvaluatorError.MemoryAllocation;
    const errorPtr = allocator.create(object.Object) catch return EvaluatorError.MemoryAllocation;
    errorPtr.* = object.Object{ .error_ = object.Error{ .message = message } };
    return errorPtr;
}

pub fn newInteger(allocator: std.mem.Allocator, value: i64) !*object.Object {
    const integerPtr = allocator.create(object.Object) catch return EvaluatorError.MemoryAllocation;
    integerPtr.* = object.Object{
        .integer = object.Integer{ .value = value },
    };
    return integerPtr;
}

pub fn newString(allocator: std.mem.Allocator, value: []const u8) !*object.Object {
    const stringPtr = allocator.create(object.Object) catch return EvaluatorError.MemoryAllocation;
    stringPtr.* = object.Object{
        .string = object.String{ .value = value },
    };
    return stringPtr;
}

pub fn newArray(allocator: std.mem.Allocator, items: std.ArrayList(*object.Object)) !*object.Object {
    const arrayPtr = allocator.create(object.Object) catch return EvaluatorError.MemoryAllocation;
    arrayPtr.* = object.Object{
        .array = object.Array{ .items = items },
    };
    return arrayPtr;
}

pub fn newFunction(allocator: std.mem.Allocator, parameters: std.ArrayList(ast.Identifier), body: *ast.Block, env: *Environment) !*object.Object {
    const functionPtr = allocator.create(object.Object) catch return EvaluatorError.MemoryAllocation;
    functionPtr.* = object.Object{
        .function = object.Function{ .parameters = parameters, .body = body, .env = env },
    };
    return functionPtr;
}
