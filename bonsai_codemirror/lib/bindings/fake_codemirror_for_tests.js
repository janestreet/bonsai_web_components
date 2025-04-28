function createSelfReturningProxy(name) {
    return new Proxy(function () { }, {
        get(target, prop) {
            // Special case just for Symbol.toPrimitive
            if (prop === Symbol.toPrimitive) {
                return () => name;
            } else if (prop === "isMockForTesting") {
                return "isMockForTesting";
            } else if (typeof prop === "string" && prop.endsWith("Keymap")) {
                // Some keymaps are resolved at the toplevel.
                // This is extremely hacky.
                return [];
            }

            // All other property accesses should return the keymap itself.
            return createSelfReturningProxy("name." + prop.toString());
        },

        set(target, prop, value) {
            throw new Error("BUG: tried to set property on mocked Codemirror instance: " + name.toString());
        },

        apply(target, thisArg, args) {
            throw new Error("BUG: tried to call function on mocked Codemirror instance: " + name.toString());
        }
    });
}

// We want to be able to run tests that depend on [codemirror_bindings] without the inline
// runner crashing at startup. But we don't want to introduce this mock if we're not running
// tests.
if (globalThis.process && globalThis.process.env && globalThis.process.env["TESTING_FRAMEWORK"]) {
    codemirror = createSelfReturningProxy("codemirror");
}
