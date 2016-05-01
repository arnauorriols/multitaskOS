Why this journal?
=================

With Elm you don't need tests. Nevertheless, TDD is not just about writting tests. TDD is about designing your product one step at a time. Ask yourself
the minimal behaviour addition or change that would make your code *move*, and then make your code move the least enough just to meet that change.

This journal operates as the 'test' part of the equation: the feature evolution that step by step made the product evolve to what it is at each particular
point of its life. 

The product is born
-------------------

1. I can tell the OS which task I am doing now.
2. I can start and stop working on a task
3. I can pause or finish a task when stopping
4. When pausing, you are actually yielding execution of the current task
5. When I yield, the OS tells me the next task I should work on
6. I can schedule new tasks to be put to the tasks queue at any time
7. When prompted to start working on the next task, I can choose to skip it
8. I can see the details of the next/ongoing task in a panel
9. When working on a task, I can log the work done in a journal
10. I can use enter to schedule new tasks or save worklog to journal
11. I can drop a task when prompted to start working on it
