const std = @import("std");

const string = @import("string");

const ast = @import("ast.zig");
const environment = @import("environment.zig");
const builtin = @import("builtin.zig");

pub const Object = union(enum) {
    null_: Null,
    integer: Integer,
    boolean: Boolean,
    string: String,
    array: Array,
    hash: Hash,
    function: Function,
    builtinFunction: BuiltinFunction,
    quote: Quote,
    macro: Macro,
    return_: Return,
    error_: Error,

    pub fn toString(self: *Object, buf: *string.String) string.String.Error!void {
        switch (self.*) {
            .null_ => |null_| try null_.toString(buf),
            .integer => |integer| try integer.toString(buf),
            .boolean => |boolean| try boolean.toString(buf),
            .string => |str| try str.toString(buf),
            .array => |*array| try array.toString(buf),
            .hash => |hash| try hash.toString(buf),
            .function => |*function| try function.toString(buf),
            .builtinFunction => |function| try function.toString(buf),
            .quote => |*quote| try quote.toString(buf),
            .macro => |*macro| try macro.toString(buf),
            .return_ => unreachable,
            .error_ => |error_| try error_.toString(buf),
        }
    }

    pub fn typeName(self: Object) []const u8 {
        return switch (self) {
            .null_ => "Null",
            .integer => "Integer",
            .boolean => "Boolean",
            .string => "String",
            .array => "Array",
            .hash => "Hash",
            .function => "Function",
            .builtinFunction => "BuiltinFunction",
            .quote => "Quote",
            .macro => "Macro",
            .return_ => unreachable,
            .error_ => "Error",
        };
    }
};

pub const Array = struct {
    items: std.ArrayList(*Object),

    pub fn toString(self: *Array, buf: *string.String) string.String.Error!void {
        try buf.concat("[");
        var i: usize = 0;
        while (i < self.items.items.len) : (i += 1) {
            try self.items.items[i].toString(buf);
            if (i != self.items.items.len - 1) {
                try buf.concat(",");
            }
        }

        try buf.concat("]");
    }
};

pub const Integer = struct {
    value: i64,

    pub fn new(value: i64) Integer {
        return .{
            .value = value,
        };
    }

    pub fn toString(self: Integer, buf: *string.String) string.String.Error!void {
        const intString = try std.fmt.allocPrint(buf.allocator, "{}", .{self.value});
        try buf.concat(intString);
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn toString(self: Boolean, buf: *string.String) string.String.Error!void {
        if (self.value) {
            try buf.concat("true");
        } else {
            try buf.concat("false");
        }
    }
};

pub const String = struct {
    value: []const u8,

    pub fn toString(self: String, buf: *string.String) string.String.Error!void {
        try buf.concat(self.value);
    }
};

pub const BuiltinFunction = struct {
    function: builtin.BuiltinFunction,

    pub fn call(self: BuiltinFunction, allocator: std.mem.Allocator, args: std.ArrayList(*Object)) !*Object {
        return try self.function.call(allocator, args);
    }

    pub fn toString(self: BuiltinFunction, buf: *string.String) string.String.Error!void {
        try buf.concat("BuiltinFunction#");
        try buf.concat(@tagName(self.function));
    }
};

pub const Error = struct {
    message: []const u8,

    pub fn toString(self: Error, buf: *string.String) string.String.Error!void {
        try buf.concat("Error: ");
        try buf.concat(self.message);
    }
};

pub const Return = struct {
    value: *Object,
};

pub const Function = struct {
    parameters: std.ArrayList(ast.Identifier),
    body: *ast.Block,
    env: *environment.Environment,

    pub fn toString(self: *Function, buf: *string.String) string.String.Error!void {
        try buf.concat("fn");
        try buf.concat("(");
        var i: usize = 0;
        while (i < self.parameters.items.len) : (i += 1) {
            try buf.concat(self.parameters.items[i].value);
            if (i != self.parameters.items.len - 1) {
                try buf.concat(", ");
            }
        }
        try buf.concat(") ");
    }
};

pub const HashableObject = union(enum) {
    integer: Integer,
    boolean: Boolean,
    string: String,

    pub fn fromObject(object: Object) HashableObject {
        return switch (object) {
            .integer => |integer| .{ .integer = integer },
            .boolean => |boolean| .{ .boolean = boolean },
            .string => |str| .{ .string = str },
            else => unreachable,
        };
    }

    pub fn toString(self: HashableObject, buf: *string.String) string.String.Error!void {
        switch (self) {
            .integer => |integer| try integer.toString(buf),
            .boolean => |boolean| try boolean.toString(buf),
            .string => |str| try str.toString(buf),
        }
    }
};

pub const Hash = struct {
    pub const HashMap = std.HashMap(
        HashableObject,
        *Object,
        HashContext,
        std.hash_map.default_max_load_percentage,
    );

    pub const HashContext = struct {
        pub fn eql(_: HashContext, obj1: HashableObject, obj2: HashableObject) bool {
            return switch (obj1) {
                .integer => |integer| switch (obj2) {
                    .integer => |integer2| integer.value == integer2.value,
                    else => false,
                },
                .boolean => |boolean| switch (obj2) {
                    .boolean => |boolean2| boolean.value == boolean2.value,
                    else => false,
                },
                .string => |str1| switch (obj2) {
                    .string => |str2| std.mem.eql(u8, str1.value, str2.value),
                    else => false,
                },
            };
        }

        pub fn hash(_: HashContext, obj: HashableObject) u64 {
            return switch (obj) {
                .integer => |integer| (std.hash_map.AutoContext(i64){}).hash(integer.value),
                .boolean => |boolean| (std.hash_map.AutoContext(bool){}).hash(boolean.value),
                .string => |str| (std.hash_map.StringContext{}).hash(str.value),
            };
        }
    };

    pairs: HashMap,

    pub fn get(self: Hash, key: HashableObject) ?*Object {
        return self.pairs.get(key);
    }

    pub fn toString(self: Hash, buf: *string.String) string.String.Error!void {
        try buf.concat("{");
        var it = self.pairs.iterator();
        const len = self.pairs.count();
        var i: usize = 0;
        while (it.next()) |pair| {
            try pair.key_ptr.*.toString(buf);
            try buf.concat(": ");
            try pair.value_ptr.*.toString(buf);
            if (i != len - 1) {
                try buf.concat(", ");
            }

            i += 1;
        }
    }
};

pub const Null = struct {
    pub fn toString(_: Null, buf: *string.String) string.String.Error!void {
        try buf.concat("null");
    }
};

pub const Quote = struct {
    node: ast.Node,

    pub fn toString(self: *Quote, buf: *string.String) string.String.Error!void {
        try self.node.toString(buf);
    }
};

pub const Macro = struct {
    parameters: std.ArrayList(*ast.Identifier),
    body: *ast.Block,
    environment: *environment.Environment,

    pub fn toString(self: *Macro, buf: *string.String) string.String.Error!void {
        try buf.concat("macro");
        try buf.concat("(");
        var i: usize = 0;
        while (i < self.parameters.items.len) : (i += 1) {
            try buf.concat(self.parameters.items[i].value);
            if (i != self.parameters.items.len - 1) {
                try buf.concat(", ");
            }
        }
        try buf.concat(") ");
    }
};
