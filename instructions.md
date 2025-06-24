# Rules

Follow the [Unison Programming Language Guide](./unison-language-guide.md)

Use the Unison programming language for all code unless otherwise specified.

Use the following procedure to assist me with writing code.

## Step 1, before writing any code: confirm types and signatures

0. If code involves new data types or abilities, confirm the data declarations with me before proceeding.
1. Confirm type signatures with me before generating any code.
2. If possible, suggest a few simple examples of inputs and outputs for the function being implemented. Confirm that these are what I expect, then add these as a commented out test> watch expression. We will uncomment them later.

Do not proceed to the next step until both these are confirmed.

## Step 2: see if similar functions exist

Using the MCP server, search by type for functions on Unison Share with the required signature. You can also search for definitions by name.

You can use the MCP server to`view` to view a function or a type, and `docs` to read its docs. Use these to help find related functions to the query.

Provide links to functions on Share and if a similar function already exists, ask if I'd like to just use that, or to proceed with an implementation.

Do NOT proceed to the next step until confirmed.

## Step 3: Implementation

Now that we've agree on the signature of the function and have a few test cases, you can proceed with implementation using either the 1-SHOT or USER-GUIDED strategy, given below.

Regardless of the strategy used, code MUST typecheck before being shown to me. I do NOT want to see any code that doesn't typecheck. You will use the Unison MCP server to typecheck all code you show me.

You MAY use the MCP server to`view` to view a function or a type, and `docs` to read its docs. Use these to help with understanding how to use functions within code you are generating.

If you're writing code that works with a type, you should probably use the MCP server to view that type and its docs. 

### The 1-SHOT strategy

The 1-SHOT strategy: If a function seems simple enough, try implementing it directly. Typecheck it.

If the 1-SHOT strategy fails after two attempts to produce code that typechecks, then start over using the USER-GUIDED implementation strategy to fill in a function's implementation:

### The USER-GUIDED strategy

* Write a skeleton implementation of the function that does at most 1 level of pattern matching and calls `todo 1` or `todo 2`, `todo 3`, etc for the implementation of branches and/or helper functions. Show me this code in a markdown block if it typechecks. Ask me if it looks okay and which of the numbered `todo` you should try filling in next. Repeat.
* If after 2 attempts during any step you cannot get code to typecheck, stop and show me the previous code that typechecked. Ask me for guidance on how to proceed.
* REMEMBER: use the MCP server to view the code and docs for any definitions you're relying on in your implementation, especially if you run into trouble.

## What to do after you have something that typechecks

1. Ask me if the implementation looks good and you want me to try running the tests we disccussed, or if I have suggested changes. Repeat until I say to proceed.
2. Uncomment the tests you wrote previously and typecheck them. If the tests do not pass, try changing the implementation until they do.
3. If you can't get the tests to pass after two attempts, show me the latest code with numbered todos and ask for guidance.

## Important rules

To be clear, NEVER generate code without first confirming the type signature of the function you are writing. Only if I respond that the signature is what I want should you proceed with implementation.

To be clear, NEVER start searching for definitions before I have confirmed the signature of the function I'm looking for.

To be clear, whenever showing me code with todos in it, show me it as a markdown block. I cannot read what you send to the MCP server.

When asking me a question, preface it with *Question:*, in bold, and put it at the bottom of any other output you've generated. This makes it easier for me to scan for. Do NOT include questions for me at random within a bunch of other output.
