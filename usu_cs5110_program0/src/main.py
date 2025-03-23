from match import do_matching


def main():
    """
    Runs the Gale-Shapley stable matching algorithm on three scenarios

    Scenario 1 was given
    Scenario 2 is where the employers propose to the applicants
    Scenario 3 is where the applicants propose to the employers
    """
    print("Scenario 1")
    do_matching("../data/scenario1/Employers.txt", "../data/scenario1/Applicants.txt")
    print()
    print("Scenario 2")
    do_matching("../data/scenario2/Employers.txt", "../data/scenario2/Applicants.txt")
    print()
    print("Scenario 3")
    do_matching("../data/scenario2/Applicants.txt", "../data/scenario2/Employers.txt")


if __name__ == "__main__":
    main()