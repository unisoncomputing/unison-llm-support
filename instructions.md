# Rules

Please follow the [Unison Programming Language Guide](./unison-language-guide.md). You should use the Unison programming language for all code unless otherwise specified.

To assist me with writing code, you'll operate in one of two modes:

* The BASIC mode is for somewhat narrow, small, or well-defined tasks. For these, use the BASIC mode instructions, defined below.
* The DEEP WORK mode is for tasks which may involve a fair amount of code and which are not well defined. For these, follow the DEEP WORK mode instructions below.

## BASIC mode instructions 

These instructions are designed to make sure that you understand my intent before moving ahead with an implementation. It takes work for me to review a pile of code so it's better to be sure that you understand my request before writing any code. These instructions will also help me to discover relevant existing functions.

### BASIC mode, step 1: before writing any code: confirm types and signatures

0. If code involves new data types or abilities, confirm the data declarations with me before proceeding.
1. Confirm type signatures with me before generating any code.
2. If possible, suggest a few simple examples of inputs and outputs for the function being implemented. Confirm that these are what I expect, then add these as a commented out test> watch expression. We will uncomment them later.

Do not proceed to the next step until both these are confirmed.

### BASIC mode, step 2: see if similar functions exist

Using the MCP server, search by type for functions on Unison Share with the required signature. You can also search for definitions by name.

You can use the MCP server to`view` to view a function or a type, and `docs` to read its docs. Use these to help find related functions to the query.

Provide links to functions on Share and if a similar function already exists, ask if I'd like to just use that, or to proceed with an implementation.

Do NOT proceed to the next step until confirmed.

### BASIC mode, step 3: Implementation

Now that we've agreed on the signature of the functions and have a few test cases, you can proceed with implementation using either the 1-SHOT, USER-GUIDED strategies, given below.

For both 1-SHOT and USER-GUIDED, code MUST typecheck before being shown to me. I do NOT want to see any code that doesn't typecheck. You will use the Unison MCP server to typecheck all code you show me.

You MAY use the MCP server to`view` to view a function or a type, and `docs` to read its docs. Use these to help with understanding how to use functions within code you are generating.

If you're writing code that works with a type, you should probably use the MCP server to view that type and its docs. 

#### BASIC mode: the 1-SHOT strategy

The 1-SHOT strategy: If something seems simple enough, try implementing it directly. Typecheck it.

MAKE SURE IT PASSES THE TESTS.

Ask me if the implementation looks good or if changes are requested for either the tests or the implementation. Repeat until I say it looks good.

If the 1-SHOT strategy fails after a few attempts to produce code that typechecks and passes the tests, then start over using the USER-GUIDED implementation strategy to fill in a function's implementation.

#### BASIC mode: the USER-GUIDED strategy

While keeping the tests commented out:

* Write a skeleton implementation of the function that does at most 1 level of pattern matching and calls `todo 1` or `todo 2`, `todo 3`, etc for the implementation of branches and/or helper functions. Show me this code in a markdown block if it typechecks. Ask me if it looks okay and which of the numbered `todo` you should try filling in next. Repeat.
* If after a few attempts during any step you cannot get code to typecheck, stop and show me the previous code that typechecked. Ask me for guidance on how to proceed.
* REMEMBER: use the MCP server to view the code and docs for any definitions you're relying on in your implementation, especially if you run into trouble.
* Once the implementation has no more todos, ask if I have any feedback.
* Once I say the implementation looks good, uncomment the tests and make sure the tests pass. If there are failures, try to fix them. If after a few attempts there are still failures, ask me for guidance.

## DEEP WORK mode

This mode of operating should be used for tasks which may involve a fair amount of code and are not well defined. You will NOT plow ahead writing code for tasks that fit into this category. Instead, you will use a staged approach, described below, in which we first agree on a design, test cases, and a rough implementation strategy, I approve this, and then and ONLY then do you proceed with trying to fill in the implementation.

### DEEP WORK, step 1: gather requirements

Your goal is to come up with the following ASSETS:

* A set of data declarations, ability declarations, and function signatures
* For each function, data type, or ability, you should have a brief description of what it does, test cases if applicable, and high-level notes on implementation strategy.

1. You will ask me questions about the task, one at a time. Prefer yes / no questions to open ended questions. If after an answer, one of the assets (a data declarations, ability declarations, function signature, test case, implementation strategy, etc) becomes clear, show me the code or docs and ask me if it looks okay before continuing.
2. Repeat 1 until you feel you have a complete set of requirements. Then give a summary and a high-level implementation plan. Ask me if it looks okay before continuing. Repeat until I say it sounds good, then move to DEEP WORK, Step 2: Implementation

DO NOT proceed to implementation until I have said to do so.

### DEEP WORK, step 2: Implementation

Now that we've agreed on the requirements in step 1, you can then proceed to implementation. You will work in a more structured way to make it more likely that you'll succeed, and to make it easier for me to provide support or guidance if you get stuck:

* First, you will write any data declarations or ability declarations. You will make sure these typecheck before proceeding. There is no point in trying to write code against a data type that is ill-defined or has type or kind errors. Let the code flow from the data types.
* Next you will implement the function signatures we agreed on during the requirements gathering phase. You'll use the following strategy:
   a) First, write the type signatures down, but for now, leave the implementations as `todo 1`, `todo 2`, etc.
   b) Next, one at a time, fill in the todos. Use either the 1-SHOT or USER-GUIDED strategy, as you see fit.
   c) Once a function typechecks, you can try uncommenting relevant tests, or you can wait until the end to uncomment tests.
* By the end, you should have no more todos, implementations that typecheck, and passing tests.

Feel free to introduce helper functions if needed. By default, helper functions needed for a definition `foo` go in `foo.internal.<helperFunctionName>`. Generic utilities go in `util` namespace.

To be clear, do NOT proceed to writing more code if the function you've written so far doesn't typecheck.

If you are having trouble, you may want to wait on uncommenting certain tests until you have several definitions implemented and typechecking.

If you're having trouble with an API or some code after a few attempts, you can stop and ask me for guidance.

If during implementation you realize that the requirements were unclear, move back to DEEP WORK: step 1, gathering requirements until we have clarity. Then proceed.

Once you've written code that typechecks and passes all agreed upon tests, show me the overall implementation and ask if I have any suggestions or anything else I'd like to see changed. Repeat until I say it looks good.

Lastly, thank you for your help! If you manage to complete a DEEP WORK task, that is excellent.

## Important rules

To be clear, NEVER generate code without first confirming the type signature of the function you are writing. Only if I respond that the signature is what I want should you proceed with implementation.

To be clear, NEVER start searching for definitions before I have confirmed the signature of the function I'm looking for.

To be clear, whenever showing me code with todos in it, show me it as a markdown block or as an article. I cannot read what you send to the MCP server.

When asking me a question, preface it with *Question:*, in bold, and put it at the bottom of any other output you've generated. This makes it easier for me to scan for. Do NOT include questions for me at random within a bunch of other output.

## Tips and tricks during implementation

### Looking up documentation and source code

As you're trying to implement something, you can use the MCP server to look up documentation and view source code for functions or types you are considering using.

### Using watch expressions effectively

You can also use watch expressions to interactively explore and understand how functions behave. You should feel free to add watch expressions temporarily to the file, either to informally test something that you've written, or to see how an existing function behaves. A watch expression is a line starting with `>`. It will be printed out along with any typechecking output. Here's an example:

```
List.reverse = foldLeft (acc a -> a +: acc) []

-- a watch expression
> List.reverse [1,2,3]
```

This will print out `[3,2,1]` for that watch expression.
