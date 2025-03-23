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
    total_questions: int = len(questions)
    correct_count: int = 0
    incorrect_count: int = 0

    while questions:
        clear_screen()
        current_question = questions.pop(0)

        print(Fore.CYAN + f"Question: {total_questions - len(questions)}/{total_questions} "
                          f"{Fore.GREEN}Correct: {correct_count} {Fore.RED}Incorrect: {incorrect_count}" + Style.RESET_ALL)

        options: List[str] = current_question["distractors"] + current_question["answer"]
        shuffle(options)

        selected_indices = question_prompt(f"{current_question['prompt']}", options)

        # Extract selected answers
        selected_answers = [options[i] for i in selected_indices]
        correct_answers = current_question["answer"]

        # Check if any selected answers are correct
        if any(ans in selected_answers for ans in correct_answers):
            correct_count += 1
            print(Fore.GREEN + "\nCorrect!")
            input("Press Enter to continue...")
        else:
            incorrect_count += 1
            print(Fore.RED + "\nIncorrect: ", end="")
            print(f"The correct answer(s): {', '.join(correct_answers)}")

            retry: str = input(Fore.YELLOW + "Do you want to try this question again later? (yes/no): " + Style.RESET_ALL).strip().lower()
            if retry in ["yes", "y"]:
                questions.append(current_question)
                total_questions += 1
        print(Fore.RESET)

    score: int = correct_count / total_questions
    print(f"Your score: {score * 100:.2f}%")


def question_prompt(prompt: str, options: List[str]) -> List[int]:
    print()
    print(Fore.CYAN + prompt + Style.RESET_ALL)
    for i, option in enumerate(options):
        print(f"\t{Fore.YELLOW}{i + 1}: {option}{Style.RESET_ALL}")
    print()
    while True:
        choice: str = input(Fore.MAGENTA + "Your choice (numbers separated by spaces): " + Style.RESET_ALL)
        if choice.lower() == "exit":
            print("Exiting the program.")
            exit()

        # Split input and convert to integers
        choices = choice.split()
        if all(c.isdigit() and 1 <= int(c) <= len(options) for c in choices):
            return [int(c) - 1 for c in choices]  # Return a list of selected indices
        else:
            print(Fore.RED + "Invalid choice. Please select valid options." + Style.RESET_ALL)


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
