import os
import platform
import json
from random import randint, shuffle
from colorama import Fore

def main():
    file = load_questions()

    options = [key for key in file.keys()]
    response = question(f"Choose a question group", options)
    section = file[options[response]]

    test(section)


def validate_options(pick: str, valid: list) -> int | None:
    if pick in valid:
        return valid.index(pick)
    elif pick.isdigit() and int(pick) < len(valid) and int(pick) >= 0:
        return int(pick)
    return None


def question(prompt: str, options: list) -> int:
    clear_screen()
    print(prompt + "\n")
    [print(f"{i}. {val}") for i, val in enumerate(options)]
    pick = input("\n-> ")

    if pick == "exit":
        exit()

    response = validate_options(pick, options)
    return response if response is not None else question(prompt, options)


def test(section: dict):
    answers = []

    questions = list(section.keys())
    shuffle(questions)
    for key in questions:
        options = section[key]["distractors"] + section[key]["answer"]
        response = question(f"{key}. {section[key]["prompt"]}", options)

        if options[response] in section[key]["answer"]:
            answers.append(1)
            print(Fore.GREEN, "\nCorrect!")
        else:
            answers.append(0)
            print(Fore.RED, "\nIncorrect: ", end="")
            print(f"The correct answer is {section[key]["answer"]}")
            print(Fore.RESET)

        print(Fore.RESET)
        print("Press enter for next question...")
        input()
        clear_screen()

    score = 0
    for ans in answers:
        score += ans

    print(f"Your score: {score / len(answers)}")


def load_questions():
    with open('questions.json', 'r') as file:
        return json.load(file)


def clear_screen():
    if platform.system() == 'Linux':
        os.system('clear')
    else:
        os.system('cls')

if __name__ == "__main__":
    main()
