# Absolute vs. Relative paths

*TL;DR* Write programs such that the user is fully responsible for specifying
the path to input files.  Your program should *not* encode any information
about files or directories on your computer.  It is too much information (TMI)
and makes your program less portable.


#### Path

Specifies a location in a hierarchical file system as a sequence of directories
leading from the beginning or the "root"


#### Current Working Directory (CWD)

The directory a process is running in.

Every process which runs on your computer runs in a "location" on your file
system.  For some processes, such as a command shell or a file explorer this
location is very obvious to the user.  For other processes this location is
less visible and may not matter.

Processes can launch other processes; the command shell is one example of a
process that launches other processes, the Start menu is another.

*Child* processes inherit some attributes from their *parent* process.  The CWD
is an inheritable attribute.  When you run a program from the shell, both the
shell and the child process share the same working directory.


#### Absolute path

A path that completely describes the location of a file or directory.

An absolute path tells a process how to locate a file anywhere on the system at
any time regardless of its current working directory.


Examples:

*   `C:\Users\Erik Falor\Desktop\New Text Document.txt`
*   `/home/fadein/1440`


#### Relative path

An incomplete path specification which must be combined with a program's CWD to
locate a file or directory.

A relative path is added to a program's current directory to form an absolute
path.  This lets a program locate a file in a location relative to its CWD.

Examples:

*   `..\..\Old Text Document.txt`
*   `../data/msg1.txt`


## Laziness and paths

Every time your computer uses a file it **needs** an absolute path.  Because
every process on your computer already has a CWD you are not obligated to
always spell out an absolute path; relative paths are automatically combined
with the CWD for you by the Operating System.


## File path best practices by example

Unless I go out of my way to tell you otherwise, please *never* hard code paths
into your programs.  The reason for this is illustrated below:


### Bad:

    f = open("C:\\Users\\Mr.CoolGuy\\school\\cs1440\\homeworks\\cs1440-falor-erik-assn4\\data" + sys.argv[1])

*Problem*: This directory doesn't exist on the graders' computers, so your
program will always fail for them.  They must edit your code in order to simply
run the program.


### Bad:

    f = open("../data/" + sys.argv[1])

*Problem*: This program only works when I launch it from `src/` or some other
sibling directory to `data/`.  The graders must open your program in an editor
to figure out where it's looking for files.


### Bad:

    filename = input("Gimme the name of a file: ")
    if os.access("../" + filename, os.R_OK):
        f = open("../" + filename)
        for line in f:
            pass
    else:
        print("Unable to access ", filename)

*Problem*: Although this code checks that a file is accessible before
attempting to open it, it still imposes an expectation about the location of
the file on the user.

While this program doesn't crash, it also doesn't help the user correct the
problem.  The user cannot learn from this error message where the program is
looking for files.  When the user investigates they find that the file indeed
exists at the specified path *and* is accessible.  The error message doesn't
indicate that the program expects to find files in a location relative to
where the program started.


### Good:

    f = open(sys.argv[1])

*Virtues*: Short and sweet.  Leave the details of where files are to the user,
who knows best.

Write your programs with the expectation that the user is fully responsible for
specifying the path to input files.  When you must break this expectation,
document it so that the user isn't frustrated by your choice.
