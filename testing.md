# TESTING mode

## Requirement: load relevant context

Unison has built-in testing support:

```
test> Nat.tests.props = test.verify do
  Each.repeat 100
  n = Random.natIn 0 1000
  m = Random.natIn 0 1000
  labeled "addition is commutative" do 
    ensureEqual (n + m) (m + n)
  labeled "zero is an identity for addition" do
    ensureEqual (n + 0) (0 + n)
  labeled "multiplication is commutative" do
    ensureEqual (n * m) (m * n)
```

REQUIREMENT: Tests for a function or type `foo` should always be named `foo.tests.<test-name>`.

To learn more about testing, first read https://www.unison-lang.org/docs/usage-topics/testing/

Then, using the MCP server, read the documentation for the following functions:

* `test.verify` - the main testing function
* `ensureEqual` - asserts equality
* `ensure` - asserts a `Boolean`
* `ensuring` - lazily asserts a `Boolean`
* `labeled` - adds a label to a test
* `test.arbitrary.nats` - picks a random `Nat`
* the `Each` ability 
* the `Random` ability
* `Random.natIn` - picks a random `Nat` within a range
* `Random.listOf` - generates a list of random elements

You man optionally view the source code for any of these definitions.

DO NOT proceed until you have all this context.

Then follow the instructions below:

## Instructions

1. Echo this list of instructions, verbatim.
2. Unless I have already specified, ask me what we are testing.
3. Use LEARN mode to learn about the types and functions that will be involved.
4. Propose some simple tests with input and expected output. No need for the code to typecheck yet, we're just trying to get on the same page. Wait for my approval before proceeding. I may ask you to come up with more examples.
5. Next, we will try to come up with property-based tests. You should propose ideas for property-based tests and ask me if I approve of each or have feedback or another idea. Don't proceed to the next idea until I've approved. Don't write the test yet. Again, we're just trying to agree on the ideas. Repeat until I say to move on.
6. Summarize the testing plan for me and ask if I approve or have feedback or implementation tips. Once approved, move on to implementation.

During steps 4 and 5, I may instruct you to use an I/O test rather than a pure test. Pure tests are just `test>` watch expressions in the file. 

An I/O test is named with the same convention, but is not a watch expression. It will generally look like `foo.tests.exampleTest = do test.verify do ...`. Notice that it starts with `do`, before the `test.verify` call. A pure test doesn't start with `do`.

In general, it is much preferred to use a pure test, unless the thing being tested necessarily uses `IO`.

### Implementation

Ask me if you should create a new file (you can propose a name) or add the tests to an existing file. Then follow these instructions. 

1. Output these instructions, verbatim.
2. Place the summary of the testing plan from step 6 above as a comment in the file. Use a block comment, which is surrounded by: {- -}, like so {- a comment -}
3. Write the tests planned in step 6 above. DO THEM ONE AT A TIME. Requirement: typecheck each test before moving to the next one.
4. Once all tests are complete, ensure that the file typechecks.
5. Consolidate tests with similar setup, as described below
6. Ask me for any feedback on the tests. 

#### Consolidating tests

After your tests are finished, you MUST do a consolidation pass. The idea of this is to avoid duplicating setup code. Tests that rely on the same setup or input generation should be grouped together into a single test. Use `labeled` to annotate subtests.

For simple hardcoded example tests, these are conventionally called: `foo.tests.examples` for the tests of the function `foo`. 

For property-based tests, these are conventionally called: `foo.tests.props` for the tests of the function `foo`. If there are multiple property-based tests with different setup, you can call these `foo.tests.<description>Props` where you pick `<description>` as appropriate.

If a test is doing multiple things, use `labeled` to annotate subtests. Otherwise, don't bother unless you think it is needed for clarity.

## Recovering from errors

If you are getting compile errors repeatedly, use LEARN mode to learn about the functions and types you are attempting to use. Output this paragraph verbatim whenever you do this. 

If a test is not passing, you can comment it out temporarily. When you are finished and have all other tests working, tell me about the failing tests and uncomment them and ask me how to proceed.

## Requirement: DO NOT modify the test logic to get it to pass

If a test isn't passing, DO NOT modify the logic of the test without my consent. You can modify the test such that it typechecks, or comment it out temporarily while you work on other tests, but if you are changing what the test is actually testing for, that is NOT ALLOWED.

To be clear, you MUST faithfully follow the testing plan.

If a test is not passing, you can comment it out temporarily. When you are finished and have all other tests working, tell me about the failing tests and uncomment them and ask me how to proceed.
