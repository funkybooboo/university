
from centralized.graph import Graph
from multiagent.match import do_matching
from random import randint


def main():
    print("Hello! We are going to compair a centralized way of solving problems vs a multi-agent way")
    for i in range(1, 4):
        run(i, "Employers -> Applicants", f"../data/scenario{i}/Employers.txt", f"../data/scenario{i}/Applicants.txt")
        run(i, "Applicants -> Employers", f"../data/scenario{i}/Applicants.txt", f"../data/scenario{i}/Employers.txt")
    print("\n\n\n")
    print("Running Scenario4 -> Random")
    random_run(1)


def random_run(count):
    for _ in range(count):
        count1 = int((randint(3, 26) + randint(3, 26)) / 2)
        count2 = int((randint(3, 26) + randint(3, 26)) / 2)

        upper_letters = [chr(i) for i in range(65, 65+count1+1)]
        lower_letters = [chr(i) for i in range(97, 97+count2+1)]

        with open("../data/scenario4/Employers.txt", "w") as employer_file:
            fill_file(count1, employer_file, lower_letters, upper_letters)

        with open("../data/scenario4/Applicants.txt", "w") as applicant_file:
            fill_file(count2, applicant_file, upper_letters, lower_letters)

        run(4, "Employers -> Applicants", f"../data/scenario{4}/Employers.txt", f"../data/scenario{4}/Applicants.txt")
        run(4, "Applicants -> Employers", f"../data/scenario{4}/Applicants.txt", f"../data/scenario{4}/Employers.txt")


def fill_file(count, file, first_letters, second_letters):
    for l in first_letters:
        s = f"{l}:"
        n = randint(3, count)
        done = []
        for k in range(n):
            j = randint(0, len(second_letters) - 1)
            if second_letters[j] not in done:
                s += second_letters[j] + ","
            done.append(second_letters[j])
        file.write(s[:-1] + "\n")


def run(round_, direction, proposer_file_path, receiver_file_path, verbose=False):
    print()
    print(f"scenario{round_}")
    print(direction)
    print()
    print("-------------------------------------")
    print()
    print("centralized")
    print()
    central_happiness, central_count = Graph(proposer_file_path, receiver_file_path, verbose).do_flow()
    print()
    print("-------------------------------------")
    print()
    print("multi-agent")
    multi_happiness, multi_count = do_matching(proposer_file_path, receiver_file_path, verbose)
    print()
    print("-------------------------------------")
    print()
    print("Higher is better")
    print("Central Count", central_count)
    print("Multi-agent Count", multi_count)
    print()
    print("Lower is better")
    print("Central Happiness:", central_happiness)
    print("Multi-agent Happiness:", multi_happiness)
    print()
    print("-------------------------------------")
    print("-------------------------------------")
    print("-------------------------------------")
    print()


if __name__ == '__main__':
    main()