import os
import platform
import json
from random import shuffle
from typing import List, Dict, Union

from colorama import Fore, Style


def main() -> None:
    file: Dict[str, Dict[str, Union[str, List[str]]]] = load_questions()
    all_questions: List[Dict[str, Union[str, List[str]]]] = [question for question in file.values()]

    while True:
        response: str = input("How many questions? ").strip()

        if response.lower() == "exit":
            print("Exiting the program.")
            break

        if is_integer(response):
            count: int = int(response)
            if count > len(all_questions):
                print("Not enough questions available.")
                continue
        elif response == "all" or response == "":
            count = len(all_questions)
        else:
            continue

        shuffle(all_questions)  # Shuffle all the questions
        questions: List[Dict[str, Union[str, List[str]]]] = all_questions[:count]  # Get the first `count` questions

        quiz(questions)
        

def is_integer(s: str) -> bool:
    try:
        int(s)  # Try converting the string to an integer
        return True
    except ValueError:  # If a ValueError occurs, it's not a valid integer
        return False


def quiz(questions: List[Dict[str, Union[str, List[str]]]]) -> None:
    answers: List[int] = []
    total_questions: int = len(questions)  # Get the total number of questions
    correct_count: int = 0  # Counter for correct answers
    incorrect_count: int = 0  # Counter for incorrect answers

    while questions:  # Loop while there are questions left
        clear_screen()
        current_question = questions.pop(0)  # Get the first question

        # Print the number of questions answered and remaining
        print(Fore.CYAN + f"Question: {total_questions - len(questions)}/{total_questions} "
                          f"{Fore.GREEN}Correct: {correct_count} {Fore.RED}Incorrect: {incorrect_count}" + Style.RESET_ALL)

        options: List[str] = current_question["distractors"] + current_question["answer"]
        shuffle(options)  # Shuffle options for the current question
        response: int = question_prompt(f"{current_question['prompt']}", options)  # Create a function to prompt user

        if options[response] in current_question["answer"]:
            correct_count += 1  # Increment correct count
            print(Fore.GREEN + "\nCorrect!")
            # Allow the user to simply press Enter to proceed
            print(Fore.RESET)
            input("Press Enter to continue...")
        else:
            incorrect_count += 1  # Increment incorrect count
            print(Fore.RED + "\nIncorrect: ", end="")
            print(f"The correct answer is {current_question['answer'][0]}")  # Display the first correct answer

            # Prompt user to retry the question
            retry: str = input(Fore.YELLOW + "Do you want to try this question again later? (yes/no): " + Style.RESET_ALL).strip().lower()
            if retry in ["yes", "y"]:
                questions.append(current_question)  # Add question back to the list for reshuffling
                total_questions += 1  # Increase the question count
            print(Fore.RESET)

    score: int = sum(answers)  # Calculate score
    print(f"Your score: {score / len(answers) * 100:.2f}%")


def grade_response(answers: List[int], options: List[str], question: Dict[str, Union[str, List[str]]], response: int) -> None:
    if options[response] in question["answer"]:
        answers.append(1)
        print(Fore.GREEN + "\nCorrect!")
    else:
        answers.append(0)
        print(Fore.RED + "\nIncorrect: ", end="")
        print(f"The correct answer is {question['answer'][0]}")  # Display the first correct answer
        print(Fore.RESET)


def question_prompt(prompt: str, options: List[str]) -> int:
    print(Fore.CYAN + prompt + Style.RESET_ALL)  # Color the prompt
    for i, option in enumerate(options):
        print(f"\t{Fore.YELLOW}{i + 1}: {option}{Style.RESET_ALL}")  # Color the options

    while True:
        choice: str = input(Fore.MAGENTA + "Your choice (number): " + Style.RESET_ALL)
        if choice.lower() == "exit":
            print("Exiting the program.")
            exit()  # Exit the program immediately
        if choice.isdigit() and 1 <= int(choice) <= len(options):
            return int(choice) - 1  # Return index of selected option
        else:
            print(Fore.RED + "Invalid choice. Please select a valid option." + Style.RESET_ALL)


def load_questions() -> Dict[str, Dict[str, Union[str, List[str]]]]:
    with open('questions.json', 'r') as file:
        return json.load(file)


def clear_screen() -> None:
    if platform.system() == 'Linux':
        os.system('clear')
    else:
        os.system('cls')


if __name__ == "__main__":
    main()
