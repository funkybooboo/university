# CS 1440 Assignment 1: DuckieDecrypter - Rubric

| Points | Criteria
|:------:|--------------------------------------------------------------------------------
| 25     | Software Development Plan is comprehensive and well-written.<br/>Follows DuckieCorp project conventions.<br/>Other required documentation is filled out as well.
| 5      | Signature file is filled out correctly.
| 5      | Lsn0 completed and associated tests pass
| 10     | Lsn1 completed and associated tests pass
| 10     | Lsn2 completed and associated tests pass
| 20     | DuckieCrypt Decrypter Implementation (accuracy)<br/>How accurately the program decrypts the characters it needs to.
| 10     | DuckieCrypt Decrypter Implementation (behavior)<br/>How the program handles user input.

**Total points: 85**


## Penalties

*Please read the "How To Submit Assignments" page of Canvas (found under the DuckieCorp Employee Handbook Module) or in the Lecture Notes repo for more in-depth information on these penalties and what is expected.*

***Penalties specific to this assignment***:

0.  **100% point penalty** if your program imports any modules **except**:
    *   `sys`
    *   `os`
    *   `typing`
    *   Modules provided in the starter code
    *   Modules that you wrote yourself
    *   I want you to have the experience of solving these puzzles for yourself and not to leverage code written by others, no matter how "real-world" it would be to do so.

***Penalties for all assignments***:

#### Project Layout
0.  **10 point penalty** if the repository does not follow the Git Repository Naming Convention
1. **10 point penalty** if the submitted project is not a clone of the starter code repository.
2.  **10 point penalty** if there is an omission of required files and directories (missing, renamed, or moved from their expected location)
3.  **10 point penalty** if there are forbidden files and directories in the submission
4.  **10 point penalty** if there is no `.gitignore` file (whether it is missing or corrupted)
5. **Late Penalty**:
    *   \<24hrs late : 75% point cap
    *   \>=24hrs & <48hrs : 50% point cap
    *   \>=48hours : no points will be given; submissions not allowed after 48hrs

#### Modules and Functions
0. **10 point penalty** if a module fails to import due to misspelling or incorrect capitalization.
1.  **10 point penalty** if the program attempts to import a module from the `src.` package; this is the result of a PyCharm misconfiguration.
2.  **10 point penalty** `eval()` or a similar function is used by your program; use type constructor functions such as `int()` and `float()` instead
3. **\<Varies\> point penalty** A library which the grader doesn't happen to have installed is imported; The resulting `ModuleNotFoundError` is treated as a crash and penalized accordingly
4. **up to 100% point penalty** A library not permitted by the instructions is used, but doesn't result in a crash

#### Files and Paths
0.  **10 point penalty** if the program contains hard-coded paths or otherwise does not function when run from *any* CWD.  (Note: names of modules in `import` statements do not count as "hard-coded").
1.  **10 point penalty** if one or more data files are not closed after being processed in *ordinary* situations.  In the event of an error your program will display an error message and immediately exit; in such cases you do not need to take special measures to close files because they will automatically be closed as your program exits.
2.  **100% point penalty** if external programs are called upon to do the work.  Our customer has hired you to create a pure-Python solution, not a shell script that relies on external programs.
    - Do not use `os.system()`, `subprocess`, `pipes` or similar functions and libraries
    - Write a pure Python solution, not a script that leverages external programs

#### All Else
0. **Crashing Code Penalty**:
    * *Reminder: it is your responsibility to test and ensure that your program works on the graders computer*
    *   Code that is crashing and cannot be quickly & easily fixed by the grader will receive a 0% point cap penalty on the implementation portion of the rubric (0 points on implementation)
    *   Code that is crashing and CAN be quickly & easily fixed by the grader (or is only crashing some of the time) will receive a 50% point cap penalty on the implementation portion of the rubric
