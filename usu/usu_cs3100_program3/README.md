## CS3100 Program 3: Custom Command Shell (nash)

### Overview
`nash` is a custom command shell designed to provide a simplified interface for executing shell commands, 
supporting features such as command history, pipeline execution, and directory management. 
It serves as an educational project for understanding shell behavior and command execution. 
`nash` works on any operating system that has Java installed.

### Features
- **Command Execution**: Execute built-in commands and system commands.
- **Command History**: Maintain a history of commands entered for easy re-execution.
- **Pipelines**: Support command chaining using pipes (`|`).
- **Background Execution**: Ability to run commands in the background using `&`, allowing for non-blocking command execution.
- **Directory Management**: Commands for navigating and managing the file system.

### Command List
Here’s a list of available commands in `nash`:

1. **cd**: Change the current working directory.
    - Usage: `cd [directory]`
    - If no directory is specified, it changes to the user’s home directory.

2. **exit**: Terminate the shell.
    - Usage: `exit [code]`
    - Exits with the specified code; defaults to `0`.

3. **list**: List files and directories in the specified directory.
    - Usage: `list [directory]`
    - Defaults to the current directory if no argument is provided.

4. **mkdir**: Create new directories.
    - Usage: `mdir [directory...]`
    - Creates the specified directories; errors if they already exist.

5. **rmdir**: Remove empty directories.
    - Usage: `rdir [directory...]`
    - Errors if the directory is not empty or doesn’t exist.

6. **ptime**: Display cumulative execution time of child processes.
    - Usage: `ptime`
    - Shows the total time spent in child processes.

7. **history**: Show command history.
    - Usage: `history`
    - Lists all previously executed commands.

8. **clear**: Clear the terminal screen.
    - Usage: `clear`
    - Clears the output displayed in the terminal.

9. **^ (Past)**: Re-execute a command from history.
    - Usage: `^ [index]`
    - Executes the command at the specified history index.

### Architecture

#### Main Components

1. **Shell**: The entry point for the shell program. Manages user input and command execution.
    - Handles user prompts and reads commands.

2. **CommandParser**: Parses user commands into executable segments.
    - Splits commands based on the pipe character (`|`) and manages background execution.

3. **CommandExecutor**: Executes commands and handles output streams.
    - Manages both foreground and background executions.

4. **CommandFactory**: Creates command instances based on user input.
    - Maps command names to their corresponding classes.

5. **Commands**: A set of classes, each representing a specific command (e.g., `Cd`, `Exit`, `List`, etc.).

6. **Pipeline**: Facilitates the execution of command pipelines, allowing output from one command to be passed as input to the next.

### Installation
To run `nash`, ensure you have Java Development Kit (JDK) installed. Clone the repository and compile the source files. 
Execute the `Shell` class to start the shell. `nash` is compatible with any operating system that has Java installed.
You can also use the `run.sh` bash script if you have bash. Look at it for reference on how to run the code either way.

### Usage
1. Launch the shell by executing the `Shell` class.
2. Enter commands at the prompt.
3. Use `|` to chain commands together.
4. Utilize `^` to repeat commands from history.

### Example Commands
```bash
nash> list
nash> cd Documents
nash> mkdir new_folder
nash> rdir empty_folder
nash> ptime
nash> history
nash> ^ 2  # Re-executes the second command in history
nash> ls &  # Runs the 'ls' command in the background
nash> (ls | grep src) &  # Runs a pipeline in the background
```

### Error Handling
`nash` provides user-friendly error messages for invalid commands, incorrect arguments, and other execution issues.
