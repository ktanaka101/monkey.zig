const std = @import("std");

comptime {
    std.testing.refAllDecls(@This());
}

const string = @import("string");

const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const ast = @import("ast.zig");
const environment = @import("environment.zig");
const object = @import("object.zig");
const builtin = @import("builtin.zig");
const EvaluatorError = @import("evaluator_error.zig").EvaluatorError;
const util = @import("evaluator_util.zig");

pub const Evaluator = struct {
    allocator: std.mem.Allocator,

    const Self = @This();

    fn new(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
        };
    }

    fn evalNode(self: *Self, node: ast.Node, env: *environment.Environment) EvaluatorError!*object.Object {
        switch (node) {
            .program => |program| return try self.evalProgram(program, env),
            .statement => |statement| return try self.evalStatement(statement, env),
            .expression => |expression| return try self.evalExpression(expression, env),
        }
    }

    pub fn evalProgram(self: *Self, program: *ast.Program, env: *environment.Environment) EvaluatorError!*object.Object {
        var result: *object.Object = &builtin.NULL_OBJECT;
        var i: usize = 0;
        while (i < program.statements.items.len) : (i += 1) {
            const evaled = try self.evalStatement(&program.statements.items[i], env);
            switch (evaled.*) {
                .return_ => |return_| return return_.value,
                .error_ => return evaled,
                else => result = evaled,
            }
        }

        return result;
    }

    fn evalStatement(self: *Self, statement: *ast.Statement, env: *environment.Environment) EvaluatorError!*object.Object {
        switch (statement.*) {
            .expressionStatement => |expressionStatement| return self.evalExpression(expressionStatement.expression, env),
            .let => |let| {
                const value = try self.evalExpression(let.value, env);
                switch (value.*) {
                    .error_ => return value,
                    else => {},
                }

                try env.*.insert(let.name.value, value);
                return &builtin.NULL_OBJECT;
            },
            .block => |*block| return self.evalBlock(block, env),
            .return_ => |return_| {
                const value = try self.evalExpression(return_.value, env);
                switch (value.*) {
                    .error_ => return value,
                    else => {},
                }

                var objectPtr = self.allocator.create(object.Object) catch return EvaluatorError.MemoryAllocation;
                objectPtr.* = object.Object{
                    .return_ = object.Return{
                        .value = value,
                    },
                };
                return objectPtr;
            },
        }
    }

    fn evalExpression(self: *Self, expression: *ast.Expression, env: *environment.Environment) EvaluatorError!*object.Object {
        switch (expression.*) {
            .integer => |integer| return try util.newInteger(&self.*.allocator, integer.value),
            .stringLiteral => |str| return try util.newString(&self.*.allocator, str.value),
            .boolean => |boolean| return nativeBoolToBooleanObject(boolean.value),
            .array => |*array| {
                var elements = std.ArrayList(*object.Object).init(self.*.allocator);
                var i: usize = 0;
                while (i < array.elements.items.len) : (i += 1) {
                    const evaled = try self.evalExpression(&array.elements.items[i], env);
                    switch (evaled.*) {
                        .error_ => return evaled,
                        else => {},
                    }

                    elements.append(evaled) catch return EvaluatorError.MemoryAllocation;
                }
                return try util.newArray(&self.*.allocator, elements);
            },
            .index => |index| {
                const left = try self.evalExpression(index.left, env);
                switch (left.*) {
                    .error_ => return left,
                    else => {},
                }

                const idx = try self.evalExpression(index.index, env);
                switch (idx.*) {
                    .error_ => return idx,
                    else => {},
                }

                return try self.evalIndexExpression(left, idx);
            },
            .hash => |*hash| return try self.evalHashLiteral(hash, env),
            .identifier => |identifier| return try self.evalIdentifier(&identifier, env),
            .prefixExpression => |prefixExpression| {
                const right = try self.evalExpression(prefixExpression.right, env);
                switch (right.*) {
                    .error_ => return right,
                    else => {},
                }

                return try self.evalPrefixExpression(prefixExpression.operator, right);
            },
            .infixExpression => |infixExpression| {
                const left = try self.evalExpression(infixExpression.left, env);
                switch (left.*) {
                    .error_ => return left,
                    else => {},
                }

                const right = try self.evalExpression(infixExpression.right, env);
                switch (right.*) {
                    .error_ => return right,
                    else => {},
                }

                return try self.evalInfixExpression(&infixExpression.operator, left, right);
            },
            .if_ => |*if_| return try self.evalIfExpression(if_, env),
            .function => |*function| return util.newFunction(&self.allocator, function.parameters, &function.body, env),
            .call => |*call| {
                const function = try self.evalExpression(call.callee, env);
                switch (function.*) {
                    .error_ => return function,
                    else => {},
                }

                var args = std.ArrayList(*object.Object).init(self.allocator);
                var i: usize = 0;
                while (i < call.arguments.items.len) : (i += 1) {
                    const evaled = try self.evalExpression(&call.arguments.items[i], env);
                    switch (evaled.*) {
                        .error_ => return evaled,
                        else => {},
                    }

                    args.append(evaled) catch return EvaluatorError.MemoryAllocation;
                }
                return try self.applyFunction(function, args);
            },
            else => @panic("Bug: unsupported"),
        }
    }

    fn evalBlock(self: *Self, block: *ast.Block, env: *environment.Environment) EvaluatorError!*object.Object {
        var result: *object.Object = &builtin.NULL_OBJECT;
        var i: usize = 0;
        while (i < block.statements.items.len) : (i += 1) {
            const evaled = try self.evalStatement(&block.statements.items[i], env);
            switch (evaled.*) {
                .return_ => return evaled,
                .error_ => return evaled,
                else => result = evaled,
            }
        }

        return result;
    }

    fn evalPrefixExpression(self: *Self, operator: ast.Operator, right: *object.Object) EvaluatorError!*object.Object {
        switch (operator) {
            .bang => return evalBangOperatorExpression(right),
            .minus => return try self.evalMinusPrefixOperatorExpression(right),
            else => return util.newError(&self.allocator, "unknown operator: {s}{s}", .{ operator.toString(), right.typeName() }),
        }
    }

    fn evalMinusPrefixOperatorExpression(self: *Self, right: *object.Object) EvaluatorError!*object.Object {
        switch (right.*) {
            .integer => |integer| return try util.newInteger(&self.allocator, -integer.value),
            else => return util.newError(&self.allocator, "unknown operator: -{s}", .{right.typeName()}),
        }
    }

    fn evalInfixExpression(self: *Self, operator: *const ast.Operator, left: *object.Object, right: *object.Object) EvaluatorError!*object.Object {
        switch (left.*) {
            .integer => |leftInteger| {
                switch (right.*) {
                    .integer => |rightInteger| return try self.evalIntegerInfixExpression(operator, &leftInteger, &rightInteger),
                    else => return util.newError(
                        &self.allocator,
                        "type mismatch: {s} {s} {s}",
                        .{ left.typeName(), operator.toString(), right.typeName() },
                    ),
                }
            },
            .string => |leftString| {
                switch (right.*) {
                    .string => |rightString| return try self.evalStringInfixExpression(operator, &leftString, &rightString),
                    else => return try util.newError(
                        &self.allocator,
                        "type mismatch: {s} {s} {s}",
                        .{ left.typeName(), operator.toString(), right.typeName() },
                    ),
                }
            },
            else => {
                switch (operator.*) {
                    .equal => return nativeBoolToBooleanObject(self.compareObject(left, right)),
                    .notEqual => return nativeBoolToBooleanObject(!self.compareObject(left, right)),
                    else => {
                        if (std.mem.eql(u8, @tagName(left.*), @tagName(right.*))) {
                            return try util.newError(
                                &self.allocator,
                                "unknown operator: {s} {s} {s}",
                                .{ left.typeName(), operator.toString(), right.typeName() },
                            );
                        } else {
                            return try util.newError(
                                &self.allocator,
                                "type mismatch: {s} {s} {s}",
                                .{ left.typeName(), operator.toString(), right.typeName() },
                            );
                        }
                    },
                }
            },
        }
    }

    fn compareObject(self: Self, obj1: *object.Object, obj2: *object.Object) bool {
        switch (obj1.*) {
            .null_ => {
                switch (obj2.*) {
                    .null_ => return true,
                    else => return false,
                }
            },
            .integer => |integer1| {
                switch (obj2.*) {
                    .integer => |integer2| return integer1.value == integer2.value,
                    else => return false,
                }
            },
            .boolean => |boolean1| {
                switch (obj2.*) {
                    .boolean => |boolean2| return boolean1.value == boolean2.value,
                    else => return false,
                }
            },
            .string => |string1| {
                switch (obj2.*) {
                    .string => |string2| return std.mem.eql(u8, string1.value, string2.value),
                    else => return false,
                }
            },
            .array => |*array1| {
                switch (obj2.*) {
                    .array => |array2| {
                        if (array1.items.items.len != array2.items.items.len) return false;
                        var i: usize = 0;
                        while (i < array1.items.items.len) : (i += 1) {
                            if (!self.compareObject(array1.items.items[i], array2.items.items[i])) {
                                return false;
                            }
                        }

                        return true;
                    },
                    else => return false,
                }
            },
            .hash => |hash1| {
                switch (obj2.*) {
                    .hash => |hash2| {
                        if (hash1.pairs.count() != hash2.pairs.count()) return false;

                        var hashIterator = hash1.pairs.iterator();
                        while (hashIterator.next()) |entry| {
                            if (hash2.pairs.get(entry.key_ptr.*)) |pairValue2| {
                                if (!self.compareObject(entry.value_ptr.*, pairValue2)) {
                                    return false;
                                }
                            } else {
                                return false;
                            }
                        }

                        return true;
                    },
                    else => return false,
                }
            },
            .function => return @ptrToInt(obj1) == @ptrToInt(obj2),
            .builtinFunction => return @ptrToInt(obj1) == @ptrToInt(obj2),
            else => @panic("Unsupported comparison."),
        }
    }

    fn evalIntegerInfixExpression(self: *Self, operator: *const ast.Operator, left: *const object.Integer, right: *const object.Integer) EvaluatorError!*object.Object {
        switch (operator.*) {
            .plus => return util.newInteger(&self.allocator, left.*.value + right.*.value),
            .minus => return util.newInteger(&self.allocator, left.*.value - right.*.value),
            .asterisk => return util.newInteger(&self.allocator, left.*.value * right.*.value),
            .slash => return util.newInteger(&self.allocator, @divFloor(left.*.value, right.*.value)),
            .lt => return nativeBoolToBooleanObject(left.*.value < right.*.value),
            .gt => return nativeBoolToBooleanObject(left.*.value > right.*.value),
            .equal => return nativeBoolToBooleanObject(left.*.value == right.*.value),
            .notEqual => return nativeBoolToBooleanObject(left.*.value != right.*.value),
            else => return util.newError(
                &self.allocator,
                "unknown operator: Integer {s} Integer",
                .{operator.toString()},
            ),
        }
    }

    fn evalStringInfixExpression(self: *Self, operator: *const ast.Operator, left: *const object.String, right: *const object.String) EvaluatorError!*object.Object {
        switch (operator.*) {
            .plus => {
                const leftLength = left.*.value.len;
                const rightLength = right.*.value.len;
                const combinedLength = leftLength + rightLength;
                const combined = self.allocator.alloc(u8, combinedLength) catch return EvaluatorError.MemoryAllocation;
                std.mem.copy(u8, combined[0..leftLength], left.*.value);
                std.mem.copy(u8, combined[leftLength..combinedLength], right.*.value);

                const objectPtr = self.allocator.create(object.Object) catch return EvaluatorError.MemoryAllocation;
                objectPtr.* = object.Object{
                    .string = object.String{ .value = combined },
                };
                return objectPtr;
            },
            else => return util.newError(
                &self.allocator,
                "unknown operator: String {s} String",
                .{operator.toString()},
            ),
        }
    }

    fn evalIfExpression(self: *Self, ifExpression: *ast.If, env: *environment.Environment) EvaluatorError!*object.Object {
        const condition = try self.evalExpression(ifExpression.*.condition, env);
        switch (condition.*) {
            .error_ => return condition,
            else => {},
        }

        if (isTruthy(condition)) {
            return try self.evalBlock(&ifExpression.*.thenBranch, env);
        } else if (ifExpression.*.elseBranch) |*elseBranch| {
            return try self.evalBlock(elseBranch, env);
        } else {
            return &builtin.NULL_OBJECT;
        }
    }

    fn evalIdentifier(self: *Self, identifier: *const ast.Identifier, env: *environment.Environment) EvaluatorError!*object.Object {
        const value = env.*.get(identifier.*.value);
        if (value) |val| {
            return val;
        }

        if (std.mem.eql(u8, identifier.*.value, "len")) {
            return &builtin.BUILTIN_FUNCTION_LEN_OBJECT;
        } else if (std.mem.eql(u8, identifier.*.value, "first")) {
            return &builtin.BUILTIN_FUNCTION_FIRST_OBJECT;
        } else if (std.mem.eql(u8, identifier.*.value, "last")) {
            return &builtin.BUILTIN_FUNCTION_LAST_OBJECT;
        } else if (std.mem.eql(u8, identifier.*.value, "rest")) {
            return &builtin.BUILTIN_FUNCTION_REST_OBJECT;
        } else if (std.mem.eql(u8, identifier.*.value, "push")) {
            return &builtin.BUILTIN_FUNCTION_PUSH_OBJECT;
        } else if (std.mem.eql(u8, identifier.*.value, "puts")) {
            return &builtin.BUILTIN_FUNCTION_PUTS_OBJECT;
        } else {
            return util.newError(&self.allocator, "identifier not found: {s}", .{identifier.*.value});
        }
    }

    fn applyFunction(self: *Self, function: *object.Object, arguments: std.ArrayList(*object.Object)) !*object.Object {
        switch (function.*) {
            .function => |*func| {
                if (func.parameters.items.len != arguments.items.len) {
                    return util.newError(
                        &self.allocator,
                        "wrong number of arguments: want={}, got={}",
                        .{ func.parameters.items.len, arguments.items.len },
                    );
                }

                var extendedEnv = try self.extendFunctionEnv(func, arguments);
                const evaluated = try self.evalBlock(func.body, extendedEnv);
                return unwrapReturnValue(evaluated);
            },
            .builtinFunction => |builtinFunction| return try builtinFunction.call(&self.allocator, arguments),
            else => return util.newError(&self.allocator, "not a function: {s}", .{function.typeName()}),
        }
    }

    fn extendFunctionEnv(self: Self, function: *object.Function, arguments: std.ArrayList(*object.Object)) EvaluatorError!*environment.Environment {
        const envPtr = self.allocator.create(environment.Environment) catch return EvaluatorError.MemoryAllocation;
        envPtr.* = environment.Environment.newEnclose(self.allocator, function.*.env);
        var i: usize = 0;
        while (i < function.parameters.items.len) : (i += 1) {
            try envPtr.*.insert(function.parameters.items[i].value, arguments.items[i]);
        }

        return envPtr;
    }

    fn evalIndexExpression(self: *Self, left: *object.Object, index: *object.Object) EvaluatorError!*object.Object {
        switch (left.*) {
            .array => |array| {
                switch (index.*) {
                    .integer => |indexInteger| return try self.evalArrayIndexExpression(&array, &indexInteger),
                    else => return try util.newError(&self.allocator, "index operator not supported: {s}", .{index.typeName()}),
                }
            },
            .hash => |hash| return try self.evalHashIndexExpression(&hash, index),
            else => return try util.newError(&self.allocator, "index operator not supported: {s}", .{left.typeName()}),
        }
    }

    fn evalArrayIndexExpression(_: Self, array: *const object.Array, index: *const object.Integer) EvaluatorError!*object.Object {
        const arrayLength = array.*.items.items.len;
        if (index.*.value < 0 or index.*.value >= arrayLength) {
            return &builtin.NULL_OBJECT;
        }

        const idx = @intCast(usize, index.*.value);
        return array.*.items.items[idx];
    }

    fn evalHashLiteral(self: *Self, hash: *ast.Hash, env: *environment.Environment) EvaluatorError!*object.Object {
        var pairs = object.Hash.HashMap.init(self.allocator);
        var i: usize = 0;
        while (i < hash.*.pairs.items.len) : (i += 1) {
            const key = try self.evalExpression(&hash.*.pairs.items[i].key, env);
            switch (key.*) {
                .error_ => return key,
                else => {},
            }

            const value = try self.evalExpression(&hash.*.pairs.items[i].value, env);
            switch (value.*) {
                .error_ => return value,
                else => {},
            }

            pairs.put(object.HashableObject.fromObject(key.*), value) catch return EvaluatorError.MemoryAllocation;
        }

        const objectPtr = self.allocator.create(object.Object) catch return EvaluatorError.MemoryAllocation;
        objectPtr.* = object.Object{
            .hash = object.Hash{ .pairs = pairs },
        };
        return objectPtr;
    }

    fn convertToHashableFromObject(obj: object.Object) ?object.HashableObject {
        switch (obj) {
            .integer => |integer| return object.HashableObject{ .integer = integer },
            .boolean => |boolean| return object.HashableObject{ .boolean = boolean },
            .string => |str| return object.HashableObject{ .string = str },
            else => return null,
        }
    }

    fn evalHashIndexExpression(self: *Self, hash: *const object.Hash, index: *const object.Object) EvaluatorError!*object.Object {
        var hashKey = convertToHashableFromObject(index.*);
        if (hashKey) |key| {
            if (hash.*.get(key)) |value| {
                return value;
            } else {
                return &builtin.NULL_OBJECT;
            }
        } else {
            return try util.newError(&self.allocator, "unusable as hash key: {s}", .{index.typeName()});
        }
    }

    fn unwrapReturnValue(obj: *object.Object) *object.Object {
        switch (obj.*) {
            .return_ => |returnValue| return returnValue.value,
            else => return obj,
        }
    }

    fn isTruthy(obj: *object.Object) bool {
        switch (obj.*) {
            .boolean => |boolean| return boolean.value,
            .null_ => return false,
            else => return true,
        }
    }

    fn nativeBoolToBooleanObject(native: bool) *object.Object {
        if (native) {
            return &builtin.TRUE_OBJECT;
        } else {
            return &builtin.FALSE_OBJECT;
        }
    }

    fn evalBangOperatorExpression(right: *object.Object) *object.Object {
        switch (right.*) {
            .boolean => |boolean| {
                if (boolean.value) {
                    return &builtin.FALSE_OBJECT;
                } else {
                    return &builtin.TRUE_OBJECT;
                }
            },
            .null_ => return &builtin.TRUE_OBJECT,
            else => return &builtin.FALSE_OBJECT,
        }
    }
};

test "eval integer expression" {
    try testInteger(5, "5");
    try testInteger(5, "5");
    try testInteger(10, "10");
    try testInteger(-5, "-5");
    try testInteger(-10, "-10");
    try testInteger(10, "5 + 5 + 5 + 5 - 10");
    try testInteger(32, "2 * 2 * 2 * 2 * 2");
    try testInteger(0, "-50 + 100 + -50");
    try testInteger(20, "5 * 2 + 10");
    try testInteger(25, "5 + 2 * 10");
    try testInteger(0, "20 + 2 * -10");
    try testInteger(60, "50 / 2 * 2 + 10");
    try testInteger(30, "2 * (5 + 10)");
    try testInteger(37, "3 * 3 * 3 + 10");
    try testInteger(37, "3 * (3 * 3) + 10");
    try testInteger(50, "(5 + 10 * 2 + 15 / 3) * 2 + -10");
}

test "eval boolean expression" {
    try testBoolean(true, "true");
    try testBoolean(false, "false");
    try testBoolean(true, "1 < 2");
    try testBoolean(false, "1 > 2");
    try testBoolean(false, "1 < 1");
    try testBoolean(false, "1 > 1");
    try testBoolean(true, "1 == 1");
    try testBoolean(false, "1 != 1");
    try testBoolean(false, "1 == 2");
    try testBoolean(true, "1 != 2");
    try testBoolean(true, "true == true");
    try testBoolean(true, "false == false");
    try testBoolean(false, "true == false");
    try testBoolean(true, "true != false");
    try testBoolean(true, "false != true");
    try testBoolean(true, "(1 < 2) == true");
    try testBoolean(false, "(1 < 2) == false");
    try testBoolean(false, "(1 > 2) == true");
    try testBoolean(true, "(1 > 2) == false");
}

test "eval bang operator" {
    try testBoolean(false, "!true");
    try testBoolean(true, "!false");
    try testBoolean(false, "!5");
    try testBoolean(true, "!!true");
    try testBoolean(false, "!!false");
    try testBoolean(true, "!!5");
}

test "eval if else expression" {
    try testInteger(10, "if (true) { 10 }");
    try testInteger(20, "if (false) { 10 } else { 20 }");
    try testInteger(10, "if (1) { 10 }");
    try testInteger(10, "if (1 < 2) { 10 } else { 20 }");
    try testInteger(20, "if (1 > 2) { 10 } else { 20 }");
    try testInteger(10,
        \\ if (2 > 1) {
        \\   if (2 > 1) {
        \\     10
        \\   } else {
        \\     20
        \\   }
        \\ } else { 
        \\   30
        \\ }
    );
    try testInteger(20,
        \\ if (2 > 1) {
        \\   if (1 > 2) {
        \\     10
        \\   } else {
        \\     20
        \\   }
        \\ } else { 
        \\   30
        \\ }
    );
    try testInteger(30,
        \\ if (1 > 2) {
        \\   if (2 > 1) {
        \\     10
        \\   } else {
        \\     20
        \\   }
        \\ } else { 
        \\   30
        \\ }
    );
}

test "if non-execution branches, eval returns null" {
    try testNull("if (false) { 10 }");
    try testNull("if (1 > 2) { 10 }");
}

test "return statement" {
    try testInteger(10, "return 10;");
    try testInteger(10, "return 10; 9;");
    try testInteger(10, "return 2 * 5; 9;");
    try testInteger(10,
        \\ if (2 > 1) {
        \\     if (2 > 1) {
        \\       return 10;
        \\     }
        \\ }
        \\
        \\ return 20;
    );
}

test "error handling" {
    try testError(
        "type mismatch: Integer + Boolean",
        "5 + true;",
    );
    try testError(
        "type mismatch: Integer + Boolean",
        "5 + true; 5;",
    );
    try testError(
        "type mismatch: Integer + Boolean",
        "1 + 5 + true + 10;",
    );
    try testError(
        "unknown operator: -Boolean",
        "-true",
    );
    try testError(
        "unknown operator: Boolean + Boolean",
        "true + false;",
    );
    try testError(
        "unknown operator: Boolean + Boolean",
        "5; true + false; 5",
    );
    try testError(
        "unknown operator: Boolean + Boolean",
        "if (10 > 1) { true + false; }",
    );
    try testError(
        "unknown operator: Boolean + Boolean",
        \\ if (10 > 1) {
        \\   if (10 > 1) {
        \\     return true + false;
        \\   }
        \\   return 1;
        \\ }
        ,
    );
    try testError(
        "identifier not found: foobar",
        "foobar",
    );
    try testError(
        "unknown operator: String - String",
        \\ "Hello" - "World" 
        ,
    );
    try testError(
        "unusable as hash key: Function",
        \\ {"name": "Monkey"}[fn(x) { x }];
        ,
    );
}

test "eval let statement" {
    try testInteger(5, "let a = 5; a;");
    try testInteger(25, "let a = 5 * 5; a;");
    try testInteger(5, "let a = 5; let b = a; b;");
    try testInteger(15, "let a = 5; let b = a; let c = a + b + 5; c;");
}

test "eval function object" {
    try evalForTesting(null, "fn(x) { x + 2; };", struct {
        fn function(_: anytype, actual: *object.Object) !void {
            switch (actual.*) {
                .function => |*function| {
                    try std.testing.expectEqual(@as(usize, 1), function.parameters.items.len);
                    try std.testing.expectEqualStrings("x", function.parameters.items[0].value);
                    var allocator = std.heap.page_allocator;
                    var buf = string.String.init(&allocator);
                    defer buf.deinit();

                    try function.body.toString(&buf);
                    try std.testing.expectEqualStrings("{ (x + 2) }", buf.str());
                },
                else => return error.TypeMismatch,
            }
        }
    }.function);
}

test "eval function application" {
    try testInteger(5, "let identity = fn(x) { x; }; identity(5);");
    try testInteger(5, "let identity = fn(x) { return x; }; identity(5);");
    try testInteger(10, "let double = fn(x) { x * 2; }; double(5);");
    try testInteger(10, "let add = fn(x, y) { x + y; }; add(5, 5);");
    try testInteger(10, "let add = fn(x, y) { x + y; }; add(5, 5); add(5, 5);");
    try testInteger(20, "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));");
    try testInteger(5, "fn(x) { x; }(5)");
    try testInteger(
        4,
        \\ let add = fn(a, b) { a + b };
        \\ let sub = fn(a, b) { a - b };
        \\ let apply_func = fn(a, b, func) { func(a, b) };
        \\ apply_func(2, 2, add);
        ,
    );
    try testInteger(
        8,
        \\ let add = fn(a, b) { a + b };
        \\ let sub = fn(a, b) { a - b };
        \\ let apply_func = fn(a, b, func) { func(a, b) };
        \\ apply_func(10, 2, sub);
        ,
    );
}

test "function is closures" {
    try testInteger(
        5,
        \\ let new_addr = fn(x) {
        \\   fn(y) { x + y };
        \\ };
        \\ let addTwo = new_addr(2);
        \\ addTwo(3);
        ,
    );
    try testInteger(
        10,
        \\ let newAdder = fn(x, y) {
        \\   fn(z) { x + y + z };
        \\ };
        \\ let addThree = newAdder(2, 3);
        \\ addThree(5);
        ,
    );
    try testInteger(
        10,
        \\ let newAdder = fn(x, y) {
        \\   let q = x + y;
        \\   fn(z) { q + z };
        \\ };
        \\ let addThree = newAdder(2, 3);
        \\ addThree(5);
        ,
    );
}

test "eval string literal" {
    try testString(
        "Hello World!",
        \\ "Hello World!"
        ,
    );
}

test "eval string concatenation" {
    try testString(
        "Hello World!",
        \\ "Hello" + " " + "World!"
        ,
    );
}

test "builtin function #len" {
    try testInteger(
        0,
        \\ len("")
        ,
    );
    try testInteger(
        4,
        \\ len("four")
        ,
    );
    try testInteger(
        11,
        \\ len("hello world")
        ,
    );
    try testInteger(
        0,
        "len([])",
    );
    try testInteger(
        3,
        "len([1, 2, 3])",
    );
    try testError(
        "wrong number of arguments. got=2, want=1",
        "len(1, 2)",
    );
    try testError(
        "argument to 'len' not supported, got Integer",
        "len(1)",
    );
}

test "builtin function #first" {
    try testInteger(
        1,
        "first([1, 2, 3])",
    );
    try testString(
        "one",
        \\ first(["one", "two"])
        ,
    );
    try testInteger(
        1,
        "first([1])",
    );
    try testNull("first([])");
    try testBoolean(
        true,
        "let a = [1, 2, 3]; first(a); first(a) == first(a)",
    );
    try testError(
        "wrong number of arguments. got=2, want=1",
        "first([1], [2])",
    );
    try testError(
        "argument to 'first' must be ARRAY, got Integer",
        "first(1)",
    );
}

test "builtin function last" {
    try testInteger(
        3,
        "last([1, 2, 3])",
    );
    try testString(
        "two",
        \\ last(["one", "two"])
        ,
    );
    try testInteger(
        1,
        "last([1])",
    );
    try testNull("last([])");
    try testBoolean(
        true,
        "let a = [1, 2, 3]; last(a); last(a) == last(a)",
    );
    try testError(
        "wrong number of arguments. got=2, want=1",
        "last([1], [2])",
    );
    try testError(
        "argument to 'last' must be ARRAY, got Integer",
        "last(1)",
    );
}

test "builtin function rest" {
    try testIntegerArray(
        &.{ 2, 3 },
        "rest([1, 2, 3])",
    );
    try testIntegerArray(
        &.{},
        "rest([1])",
    );
    try testNull("rest([])");
    try testBoolean(
        true,
        "let a = [1, 2, 3]; rest(a); rest(a) == rest(a)",
    );
    try testIntegerArray(
        &.{ 3, 4 },
        "let a = [1, 2, 3, 4]; rest(rest(a));",
    );
    try testNull("let a = [1, 2, 3, 4]; rest(rest(a)); rest(rest(rest(rest(rest(a)))));");
    try testError(
        "wrong number of arguments. got=2, want=1",
        "rest([1], [2])",
    );
    try testError(
        "argument to 'rest' must be ARRAY, got Integer",
        "rest(1)",
    );
}

test "builtin function push" {
    try testIntegerArray(
        &.{ 1, 2, 3, 4 },
        "push([1, 2, 3], 4);",
    );
    try testIntegerArray(
        &.{ 1, 2, 3, 3 },
        "push([1, 2, 3], 3);",
    );
    try testIntegerArray(
        &.{1},
        "push([], 1)",
    );
    try testIntegerArray(
        &.{ 1, 2, 3 },
        "let a = [1, 2]; push(a, 3);",
    );
    try testIntegerArray(
        &.{ 1, 2, 3, 4 },
        "let a = [1, 2]; push(push(a, 3), 4);",
    );
    try testError(
        "wrong number of arguments. got=3, want=2",
        "push([1, 2, 3], 1, 2)",
    );
    try testError(
        "argument to 'push' must be ARRAY, got Integer",
        "push(1, 2)",
    );
}

test "eval array literal" {
    try testIntegerArray(
        &.{ 1, 2, 3 },
        "[1, 2, 3]",
    );
}

test "eval array index expression" {
    try testInteger(1, "[1, 2, 3][0]");
    try testInteger(2, "[1, 2, 3][1]");
    try testInteger(3, "[1, 2, 3][2]");
    try testInteger(1, "let i = 0; [1][i];");
    try testInteger(3, "[1, 2, 3][1 + 1];");
    try testInteger(3, "let myArray = [1, 2, 3]; myArray[2];");
    try testInteger(6, "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];");
    try testInteger(2, "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];");
}

test "return null when index out of range in array" {
    try testNull("[1, 2, 3][3]");
    try testNull("[1, 2, 3][-1]");
}

fn _testHashIndex(expected: i64, actualBase: []const u8, actual: []const u8) !void {
    var allocator = std.heap.page_allocator;
    var input = string.String.init(&allocator);
    defer input.deinit();
    try input.concat(actualBase);
    try input.concat("[");
    try input.concat(actual);
    try input.concat("]");
    try testInteger(expected, input.str());
}

test "eval hash literal" {
    const baseInput =
        \\ let two = "two";
        \\ {
        \\   "one": 10 - 9,
        \\   two: 1 + 1,
        \\   "thr" + "ee": 6 / 2,
        \\   4: 5,
        \\   true: 6,
        \\   false: 7
        \\ }
    ;

    try _testHashIndex(
        1,
        baseInput,
        \\ "one"
        ,
    );
    try _testHashIndex(
        2,
        baseInput,
        \\ "two"
        ,
    );
    try _testHashIndex(
        3,
        baseInput,
        \\ "three"
        ,
    );
    try _testHashIndex(5, baseInput, "4");
    try _testHashIndex(6, baseInput, "true");
    try _testHashIndex(7, baseInput, "false");
}

test "eval hash index expression" {
    try testInteger(
        5,
        \\ {"foo": 5}["foo"]
        ,
    );
    try testInteger(
        5,
        \\ let key = "foo"; {"foo": 5}[key]
        ,
    );
    try testInteger(
        5,
        \\ {5: 5}[5]
        ,
    );
    try testInteger(
        5,
        \\ {true: 5}[true]
        ,
    );
    try testInteger(
        5,
        \\ {false: 5}[false]
        ,
    );
    try testNull(
        \\ {"foo": 5}["bar"]
    );
    try testNull(
        \\ {}["foo"]
    );
}

test "fibonacci" {
    const input =
        \\ let fibonacci = fn(x) {
        \\   if (x == 0) {
        \\     return 0;
        \\   } else {
        \\     if (x == 1) {
        \\       return 1;
        \\     } else {
        \\       fibonacci(x - 1) + fibonacci(x - 2);
        \\     }
        \\   }
        \\ };
        \\ fibonacci(15);
    ;

    try testInteger(610, input);
}

// Test utils

fn evalForTesting(expected: anytype, actualInput: []const u8, expecting: fn (anytype, *object.Object) anyerror!void) !void {
    var allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer allocator.deinit();

    var lexer = Lexer.new(actualInput);
    var parser = Parser.new(&lexer, allocator.allocator());
    var program = try parser.parseProgram();

    var env = environment.Environment.new(allocator.allocator());

    var evaluator = Evaluator.new(allocator.allocator());
    const resultObject = try evaluator.evalProgram(&program, &env);

    try expecting(expected, resultObject);
}

fn expectNull(actual: *object.Object) !void {
    switch (actual.*) {
        .null_ => return,
        else => {
            std.debug.print("expected type Null, found type {s}\n", .{actual.typeName()});
            return error.TestExpectedNull;
        },
    }
}

fn expectInteger(expected: i64, actual: *object.Object) !void {
    switch (actual.*) {
        .integer => |integer| try std.testing.expectEqual(expected, integer.value),
        else => {
            std.debug.print("expected type Integer, found type {s}\n", .{actual.typeName()});
            return error.TestExpectedInteger;
        },
    }
}

fn expectBoolean(expected: bool, actual: *object.Object) !void {
    switch (actual.*) {
        .boolean => |boolean| try std.testing.expectEqual(expected, boolean.value),
        else => {
            std.debug.print("expected type Boolean, found type {s}\n", .{actual.typeName()});
            return error.TestExpectedBoolean;
        },
    }
}

fn expectString(expected: []const u8, actual: *object.Object) !void {
    switch (actual.*) {
        .string => |str| try std.testing.expectEqualStrings(expected, str.value),
        else => {
            std.debug.print("expected type String, found type {s}\n", .{actual.typeName()});
            return error.TestExpectedString;
        },
    }
}

fn expectIntegerArray(expected: []const i64, actual: *object.Object) !void {
    switch (actual.*) {
        .array => |array| {
            if (array.items.items.len != expected.len) {
                std.debug.print("expected array length {d}, found {d}\n", .{ expected.len, array.items.items.len });
                return error.TestExpectedIntegerArray;
            }

            var i: usize = 0;
            while (i < array.items.items.len) : (i += 1) {
                const item = array.items.items[i];
                switch (item.*) {
                    .integer => |integer| try std.testing.expectEqual(expected[i], integer.value),
                    else => {
                        std.debug.print("expected type Integer, found type {s}\n", .{item.typeName()});
                        return error.TestExpectedIntegerArray;
                    },
                }
            }
        },
        else => {
            std.debug.print("expected type Array, found type {s}\n", .{actual.typeName()});
            return error.TestExpectedIntegerArray;
        },
    }
}

fn expectError(expected: []const u8, actual: *object.Object) !void {
    switch (actual.*) {
        .error_ => |error_| try std.testing.expectEqualStrings(expected, error_.message),
        else => {
            std.debug.print("expected type Error, found type {s}\n", .{actual.typeName()});
            return error.TestExpectedError;
        },
    }
}

fn testNull(input: []const u8) !void {
    try evalForTesting(null, input, struct {
        fn function(_: anytype, actual: *object.Object) !void {
            try expectNull(actual);
        }
    }.function);
}

fn testInteger(expected: i64, input: []const u8) !void {
    try evalForTesting(expected, input, struct {
        fn function(e: anytype, actual: *object.Object) !void {
            try expectInteger(e, actual);
        }
    }.function);
}

fn testBoolean(expected: bool, input: []const u8) !void {
    try evalForTesting(expected, input, struct {
        fn function(e: anytype, actual: *object.Object) !void {
            try expectBoolean(e, actual);
        }
    }.function);
}

fn testString(expected: []const u8, input: []const u8) !void {
    try evalForTesting(expected, input, struct {
        fn function(e: anytype, actual: *object.Object) !void {
            try expectString(e, actual);
        }
    }.function);
}

fn testIntegerArray(expected: []const i64, input: []const u8) !void {
    try evalForTesting(expected, input, struct {
        fn function(e: anytype, actual: *object.Object) !void {
            try expectIntegerArray(e, actual);
        }
    }.function);
}

fn testError(expected: []const u8, input: []const u8) !void {
    try evalForTesting(expected, input, struct {
        fn function(e: anytype, actual: *object.Object) !void {
            try expectError(e, actual);
        }
    }.function);
}
