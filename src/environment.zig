const std = @import("std");

const string = @import("string");
const object = @import("object.zig");
const EvaluatorError = @import("evaluator_error.zig").EvaluatorError;

pub const Environment = struct {
    outer: ?*Environment,
    scope: std.StringHashMap(*object.Object),
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn new(allocator: std.mem.Allocator) Self {
        return .{
            .outer = null,
            .scope = std.StringHashMap(*object.Object).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn newEnclose(allocator: std.mem.Allocator, outer: *Environment) Self {
        return .{
            .outer = outer,
            .scope = std.StringHashMap(*object.Object).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn get(self: Self, key: []const u8) ?*object.Object {
        const name = self.scope.get(key);
        if (name != null) {
            return name;
        }

        if (self.outer) |outer| {
            return outer.get(key);
        } else {
            return null;
        }
    }

    pub fn insert(self: *Self, key: []const u8, value: *object.Object) !void {
        self.scope.put(key, value) catch return EvaluatorError.MemoryAllocation;
    }
};

test "get" {
    var allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer allocator.deinit();
    var value = object.Object{ .integer = object.Integer.new(10) };

    var env = Environment.new(allocator.allocator());
    try env.insert("foo", &value);
    try std.testing.expectEqual(value, env.get("foo").?.*);
}

test "get from outer scope" {
    var allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer allocator.deinit();
    var value = object.Object{ .integer = object.Integer.new(10) };

    var outerEnv = Environment.new(allocator.allocator());
    try outerEnv.insert("foo", &value);

    var env = Environment.newEnclose(allocator.allocator(), &outerEnv);
    try std.testing.expectEqual(value, env.get("foo").?.*);
}

test "get from outer scope with shadowing" {
    var allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer allocator.deinit();
    var value1 = object.Object{ .integer = object.Integer.new(10) };
    var value2 = object.Object{ .integer = object.Integer.new(20) };

    var outerEnv = Environment.new(allocator.allocator());
    try outerEnv.insert("foo", &value1);

    var env = Environment.newEnclose(allocator.allocator(), &outerEnv);
    try env.insert("foo", &value2);

    try std.testing.expectEqual(value2, env.get("foo").?.*);
}

test "insert" {
    var allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer allocator.deinit();
    var value = object.Object{ .integer = object.Integer.new(10) };

    var env = Environment.new(allocator.allocator());
    try env.insert("foo", &value);

    try std.testing.expectEqual(value, env.get("foo").?.*);
}

test "If a value is inserted with the same key, the value is overwritten." {
    var allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer allocator.deinit();
    var value1 = object.Object{ .integer = object.Integer.new(10) };
    var value2 = object.Object{ .integer = object.Integer.new(20) };

    var env = Environment.new(allocator.allocator());
    try env.insert("foo", &value1);
    try env.insert("foo", &value2);

    try std.testing.expectEqual(value2, env.get("foo").?.*);
}
