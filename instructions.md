# Rules

Please follow the [Unison Programming Language Guide](./unison-language-guide.md). You should use the Unison programming language for all code unless otherwise specified. @unison-language-guide.md

To assist me with writing code, you'll operate in one of these modes:

* The DISCOVERY mode is used when searching for libraries on Unison Share that may be helpful for a task.
* The LEARN mode is for familiarizing yourself with a library or codebase, in preparation for writing or editing code or answering questions about the library. If I ask you to learn about a library or familiarize yourself with it, use this mode. You can also choose to dynamically enter this mode as part of a coding task, if you find you are unfamiliar with 
* The BASIC mode is for somewhat narrow, small, or well-defined tasks. For these, use the BASIC mode instructions, defined below.
* The DEEP WORK mode is for tasks which may involve a fair amount of code and which are not well defined. For these, follow the DEEP WORK mode instructions below.
* The DOCUMENTING mode is for adding documentation to code

Whenever entering a mode, tell me on its own line one of:

* 🔍 Switching to DISCOVERY mode.
* ‍🐣 Switching to BASIC mode.
* 🧑‍🎓 Switching to LEARN mode.
* 🧠 Switching to DEEP WORK mode.
* 📝 Switching to DOCUMENTING mode.

## DISCOVERY mode instructions

Follow these steps to discover libraries for use:

1. Search for relevant libraries on Unison Share using the share-project-search MCP command
2. For each library that seems relevant, you can may view its README using the MCP command share-project-readme
3. If after reading the README, you think it seems relevant, provide me with a link to the library and ask if I'd like to lib-install it.

After installing it, you should ask if it's okay for you to enter LEARNING (a library) mode below so you can better assist me in writing code for that library.

## LEARNING mode instructions

### LEARNING (a library), steps:

PREREQUISITE: first, check to see if the library you're asked to learn is the current project. If so, use following instructions:

#### LEARNING (the current library) steps:

1. Tell me that you're learning about the current library.
2. Use the `docs` command to view the README of the current library. (It may be called "Readme" instead of "README")
3. Use the `list-project-definitions` command to view function signatures of all definitions in the library.
4. Use the LEARNING (single definition) steps below, as needed, to understand any of the definitions mentioned in the README and/or which are listed in `list-project-definitions`.

Work breadth-first, and don't invoke the LEARNING (single definition) procedure more than 30 times. You can always dig deeper later, as needed.

#### LEARNING (another library) steps:

1. Tell me that you're making sure the library is already installed in the project/branch. Tell me what project branch you're referring to.
2. Use the `list-project-libraries` command to find out about all the libraries installed for a project.
3. Use the `docs` command to view the README of the project. (It may be called "Readme" instead of "README"). For instance, if the library is in `alice_someproject_0_42_3`, use `docs alice_someproject_0_42_3.README` to view its README. It is important to use the fully qualified name for the README or you may accidentally read the current project's README.
4. Use the `list-library-definitions` command to view function signatures of all definitions in the library.
4. Use the LEARNING (single definition) steps below, as needed, to understand any of the definitions mentioned in the README and/or which are listed in `list-library-definitions`. 

Work breadth-first, and don't invoke the LEARNING (single definition) procedure more than 30 times. You can always dig deeper later, as needed.

### LEARNING (single definition) steps:

To learn about a single definition:

1. First, tell me that you're going to read that definition's documentation, source code, and explore related definitions via the dependency graph. Then proceed to:
2. Use `docs` MCP action to read its documentation.
3. (optional) Use `view-definitions` MCP action to view its source. 
4. (optional) Use `list-definition-dependencies` to get the dependencies of a definition. You can optionally use LEARNING (single definition) on these, if needed. 
5. (optional) Use `list-definition-dependents` to find places where a definition is used. You can optionally use LEARNING (single definition) on each of these, if needed.

Steps 3, 4 and 5 are optional. If a definition's usage is clear enough from its docs, you may stop there. You can look at its source, its dependencies, or its dependents to learn about related definitions. Generally, if I'm going to be modifying a definition or creating a related definition, I will look at the source code. If I'm just calling or using a definition, I might just read its docs and its signature. 

Example, if the definition is `List.frobnicate`, use `view-definitions List.frobnicate` and then `docs List.frobnicate`.

## BASIC mode instructions 

These instructions are designed to make sure that you understand my intent before moving ahead with an implementation. It takes work for me to review a pile of code so it's better to be sure that you understand my request before writing any code. These instructions will also help me to discover relevant existing functions.

### BASIC mode, step 1: before writing any code: confirm types and signatures

1. If code involves new data types or abilities, confirm the data declarations with me before proceeding.
2. Confirm type signatures with me before generating any code.
3. If possible, suggest a few simple examples of inputs and outputs for the function being implemented. Confirm that these are what I expect, then add these as a commented out test> watch expression. We will uncomment them later.

Do not proceed to the next step until both these are confirmed.

I may tell you to skip checks and proceed directly to implementation, but if I don't say otherwise, proceed to step 2.

### BASIC mode, step 2: see if similar functions exist

Using the MCP server, search by type for functions on Unison Share with the required signature. You can also search for definitions in the local codebase by name.

You can use the MCP server to `view` to view a function or a type, and `docs` to read its docs. Use these to help find related functions to the query.

Provide links to functions on Share and if a similar function already exists, ask if I'd like to just use that, or to proceed with an implementation.

Do NOT proceed to the next step until confirmed.

### BASIC mode, step 3: Implementation

Now that we've agreed on the signature of the functions and have a few test cases, you can proceed with implementation using either the 1-SHOT, USER-GUIDED strategies, given below.

For both 1-SHOT and USER-GUIDED, code MUST typecheck before being shown to me. I do NOT want to see any code that doesn't typecheck. You will use the Unison MCP server to typecheck all code you show me.

You MAY use the LEARNING (single definition) steps to learn about types and functions you are trying to use in your implementation. Generally, if you are writing code against a type, you should view that type and read its docs using the MCP server. 

#### BASIC mode: the 1-SHOT strategy

The 1-SHOT strategy: If something seems simple enough, try implementing it directly. Typecheck it.

MAKE SURE IT PASSES THE TESTS.

Do NOT modify the tests in order to get them to pass. If you think the tests are incorrect and want to change them, ask me first.

Once you have a typechecking implementation that passes the tests, ask me if the implementation looks good or if changes are requested for either the tests or the implementation. Repeat until I say it looks good.

If the 1-SHOT strategy fails after a few attempts to produce code that typechecks and passes the tests, then start over using the USER-GUIDED implementation strategy to fill in a function's implementation.

#### BASIC mode: the USER-GUIDED strategy

While keeping the tests commented out:

1. Write a skeleton implementation of the function that does at most 1 level of pattern matching and calls `todo 1` or `todo 2`, `todo 3`, etc for the implementation of branches and/or helper functions. Show me this code in a markdown block if it typechecks. Ask me if it looks okay and which of the numbered `todo` you should try filling in next. Repeat.
2. If after a few attempts during any step you cannot get code to typecheck, stop and show me the previous code that typechecked. Ask me for guidance on how to proceed.
3. REMEMBER: use the MCP server to view the code and docs for any definitions you're relying on in your implementation, especially if you run into trouble.
4. Once the implementation has no more todos, ask if I have any feedback.
5. Once I say the implementation looks good, uncomment the tests and make sure the tests pass. If there are failures, try to fix them. If after a few attempts there are still failures, ask me for guidance.

Example of a skeleton implementation:

```
type Tree a = Empty | Branch a [Tree a]

Tree.frobnicate : Tree a -> Nat
Tree.frobnicate t = match t with 
  Empty -> todo 1
  Branch -> todo 2
```

You would show me this code and ask me which todo to fill in next, and if I have any guidance.

## DEEP WORK mode

This mode of operating should be used for tasks which may involve a fair amount of code and are not well defined. You will NOT plow ahead writing code for tasks that fit into this category. Instead, you will use a staged approach, described below, in which we first agree on a design, test cases, and a rough implementation strategy, I approve this, and then and ONLY then do you proceed with trying to fill in the implementation.

### DEEP WORK, step 1: gather requirements

Your goal is to come up with the following ASSETS:

* A set of data declarations, ability declarations, and function signatures
* For each function, data type, or ability, you should have a brief description of what it does, test cases if applicable, and high-level notes on implementation strategy.

1. You will ask me questions about the task, one at a time. Prefer yes / no questions to open ended questions. If after an answer, one of the assets (a data declarations, ability declarations, function signature, test case, implementation strategy, etc) becomes clear, show me the code or docs and ask me if it looks okay before continuing.
2. Repeat 1 until you feel you have a complete set of requirements. Then give a summary and a high-level implementation plan. Ask me if it looks okay before continuing. Repeat until I say it sounds good, then move to DEEP WORK, Step 2: Implementation

### DEEP WORK, MANDATORY CHECKPOINT

After completing requirements gathering, you MUST:

1. State "DEEP WORK Step 1 complete"
2. Present the complete requirements summary
3. Ask: "Do you approve this design? Should I proceed to Step 2: Implementation?"
4. WAIT for explicit "yes" or "proceed" before continuing
5. If I don't explicitly approve, ask clarifying questions

### DEEP WORK, step 2: Implementation

Now that we've agreed on the requirements in step 1, you can then proceed to implementation. You will work in a more structured way to make it more likely that you'll succeed, and to make it easier for me to provide support or guidance if you get stuck:

#### Steps to follow during implementation

1. First, you will write any data declarations or ability declarations. You will make sure these typecheck before proceeding. There is no point in trying to write code against a data type that is ill-defined or has type or kind errors. Let the code flow from the data types.
2. Next you will implement the function signatures we agreed on during the requirements gathering phase. You'll use the following strategy:
   a) First, write the type signatures down, but for now, leave the implementations as `todo 1`, `todo 2`, etc.
   b) ONE AT A TIME, fill in the todos. Use either the 1-SHOT or USER-GUIDED strategy, as you see fit.
   c) Once a function typechecks, you can try uncommenting relevant tests, or you can wait until the end to uncomment tests.
   d) DO NOT start implementing the next function until the current function typechecks.
* By the end, you should have no more todos, implementations that typecheck, and passing tests.

Feel free to introduce helper functions if needed. By default, helper functions needed for a definition `foo` go in `foo.internal.<helperFunctionName>`. Generic utilities go in `util` namespace.

#### If you make mistakes

If you are having trouble, work in smaller pieces:

a) Don't write a bunch of code, then try to get it to typecheck. Write and typecheck a function at a time. 
b) If the function's implementation is big, and you can't get it all compiling at once, you can replace parts of the implementation with a call to `todo`, then ask for my help, as in the USER-GUIDED strategy.
c) NEVER write more code if the code you've just written doesn't typecheck.

You MAY want to wait on uncommenting certain tests until you have several definitions implemented and typechecking.

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

## DOCUMENTING mode

You will use this mode to add good documentation for a definition. After you've written code for me, you may ask me if I'd like you to add documentation, but you should not enter this mode without my consent.

### DOCUMENTING pure functions

To add documentation for a function that doesn't do I/O, `foo`, define a function `foo.doc` using the documentation syntax. Here is an example of good documentation:

      List.take.doc = {{

      ``List.take n list`` returns a list of the first `n` elements of `list`. For example:

      ```
      List.take 2 [1,2,3,4]
      ```

      A {List.take} of `0` elements returns the empty list:

      ```
      List.take 0 [1,2,3,4]
      ```

      A {List.take} of more elements than exist returns the original list:

      ```
      List.take 10000 [1,2,3,4]
      ```

      *Also see:* {List.drop}, {List.takeWhile}, {List.dropWhile}

      # Implementation notes

      The implementation takes `O(log n)`, using the finger tree structure
      of the {type List} type.

      }}

It should go immediately before the function it is documenting in the scratch file

So it includes:

1. A double backticked inline example, ``List.take n list`` which introduces variables referenced in the short description.
2. The short description ends with "For example:" 
3. Then one or more examples, first showing normal usage, then any relevant corner cases. If needed, include any short commentary to clarify, but where possible, let the example speak for itself. Do not give the expected output. The rendered version of the documentation will show the examples along with their evaluated results.
4. Then a short *Also see:* list, linking to other relevant functions. Only link to functions that actually exist; the documentation will be typechecked to ensure these links are valid.
5. Then an (optional) implementation notes section, which may include:
   * (Optional) The big-O notation for the asymptotics (generally only include this for core data structure functions, or implementations of algorithms where the user is likely to be wondering about the asymptotic performance)
   * (Optional) Any performance considerations to be aware of when using it.
   * (Optional) Any interesting tricks or surprises about how it's implemented. If the function is straightforward, leave this out.

You can use the MCP server to view the source of existing docs and get a sense of the syntax. Here are a few examples to look at, if you haven't already done so:

* docs List.filterMap
* docs List.drop
* view-definitions List.filterMap.doc List.map.doc List.doc Random.doc

### DOCUMENTING functions that do I/O 

Functions that use I/O cannot be evaluated inside of a documentation block, but you can still include a typechecked example showing usage. There are a couple modes you can use:

* Simple mode, if the function's usage is straightforward
* Complicated mode, if it's not straightforward and a more detailed example would be clearer

#### DOCUMENTING I/O functions, simple mode

Use a `@typechecked` block, but otherwise document things the same as you would any pure function. Here's an example, documenting the `printLine` function:

      IO.console.printLine.doc = {{

      ``printLine msg`` prints `msg` to standard out, with a newline at the end. For example:

      @typechecked ```
      printLine "Hello, world! 👋"
      printLine "Goodbye, world! 👋"
      ```
      
      *Also see:* {Handle.putText}, {Handle.stdOut}

      # Implementation notes

      If multiple threads are writing to {Handle.stdOut} concurrently, 
      you may see interleaving of output.

      }}

#### DOCUMENTING I/O functions, complicated mode

Use this mode for functions whose usage is less straightforward and would benefit from a worked example.

1. To document the function `frobnicate` which does I/O, create a definition `frobnicate.doc.example`, of type `'{IO, Exception} a` for some `a` (often `()`). This example should be self-contained so the user can try running it.
2. Instead of using a `@typechecked` block, use a `@source{frobnicate.doc.example}` block to show the source of the example, or `@foldedSource{frobnicate.doc.example}` if it's very long and you want it to be collapsed initially in the rendered version. 
3. Include a line instructing the user on how to run it, and show the output.

Here's an example:

      IO.console.printLine.doc.example = do 
         printLine "What is your name? \n> "
         name = readLine()
         printLine ("Hello there, " ++ name)
        
      IO.console.printLine.doc = {{

      ``printLine msg`` prints `msg` to standard out, with a newline at the end. For example:

      @source{IO.console.printLine.doc.example}
      
      You can run this in UCM via `run IO.console.printLine.doc.example`, and it produces
      the output:

      ```raw
      What is your name? 
      > Alice
      Hello there, Alice
      ```
      
      *Also see:* {Handle.putText}, {Handle.stdOut}

      # Implementation notes

      If multiple threads are writing to {Handle.stdOut} concurrently, 
      you may see interleaving of output.

      }}

If the output type of the example is a more interesting type than just `()`, use a `@typechecked` block for showing the output, so that it renders as hyperlinked code. For example:

     myLibrary.readFromFile : FilePath ->{IO,Exception} Map Text [Nat]
     
     myLibrary.readFromFile path = ... -- elided

     myLibrary.readFromFile.doc = {{
     
     ``readFromFile path`` reads the transmogrification parameters from the given `path`.

     @source{myLibrary.readFromFile.doc.example}

     When run in UCM via `run myLibrary.readFromFile.doc.example`, this produces output like:

     @typechecked ```
     Map.fromList [
       ("Alice", [1,2,3]),
       ("Bob", [3,2,1]),
       ("Carol", [0,1,0])
     ]
     ```
     }}

### DOCUMENTING types or abilities

Documenting types or abilities is a bit different. You should strive to give an overview of the type and the typical functions used for interacting with it. Try to give a brief overview, then more detailed sections as needed.

If you haven't already done so, you can use the MCP server to take a look at a few examples using `view-definitions List.doc Random.doc Bytes.doc`.

Not all types need this much documentation, but you can use these as inspiration.

### DOCUMENTATION RULES

* When referencing another definition, use a proper term link, like {List.map} or {Bar.qux}. This will turn into a hyperlink when rendering. Do NOT just include a backticked reference like `Bar.quaffle`, since that will just show up as monospaced font, without a link to the source code of that definition.
* When referencing a data type or ability, use a type link, as in: {type Map} or {type Random} or {type List}. This will turn into a hyperlink when rendering. Do NOT just include a backticked reference like `Map`.
* If it's useful to include type parameters when referencing a type, you can use {type Map} `k v` so that at least the `Map` part renders as a clickable hyperlink.

## REQUIREMENT: do a code cleanup pass.

After writing code that typechecks, do a cleanup pass.

### Remove needless `use` clauses and/or shorten them

If a suffix is unique, you don't need to import or write out the fully qualified name. You can just use the unique suffix without imports.

NOT PREFERRED: 

```
badCode = do 
  use lib.systemfw_volturno_0_8_0.Blah frobnicate 
  frobnicate 42 
```

PREFERRED:

```
badCode = do 
  Blah.frobnicate 42 
```

Only if you are referencing `Blah.frobnicate` a few times in the block should you bother with an import statement, and even then, try to avoid mentioning the library version number unless there are multiple definitions whose fully qualified name ends in `Blah.frobnicate`

For instance, this is okay, since `Blah.frobnicate` is being referenced several times:

```
okayCode = do
  use Blah frobnicate
  frobnicate 1
  frobnicate 2
  frobnicate 19
```

### Consider eta-reducing functions

If a function's last argument is immediately passed as the argument to another function, you can optionally eta-reduce:

BEFORE eta-reducing:

```
Nat.sum xs = List.foldLeft 0 (+) xs
```

AFTER eta-reducing:

```
Nat.sum = List.foldLeft 0 (+)
```

If a function's implementation is a single function call like this, I like to eta-reduce. If the function declares multiple bindings or is more than a handful of lines or so, I avoid it.

## REQUIREMENTS: you must output the code you've written, and that code must typecheck

ANYTIME you write code on my behalf, it needs to go to a file (you may suggest a file name), or as an artifact that I can copy/paste into a file, or if it is short, shown to me onscreen.

The code you show me / output to the file / produce as an artifact MUST typecheck. 

That is: you will first typecheck the COMPLETE code AND TESTS, then output it VERBATIM.

You will never output code that has not been typechecked.

You will never output test> watch expressions unless they have been typechecked.

To be clear, you are not done just because the code typechecks: that complete code must be shown to me, put in a file, or produced as an artifact so that I can use it. I cannot easily see the tool call invocations and the code there is not well-formatted for human consumption.
