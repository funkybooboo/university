"""
Demo of Gale-Shapley stable matching algorithm.
Written by Michael Goldwasser
Modified by Vicki Allan
Modified by Nate Stott

For simplicity, the file format is assumed (without checking) to match
the following format:

  bob:     alice,carol
  david:   carol,alice

and likewise for the applicant file,  and the identifiers should be
self-consistent between the two files.
If a match is unacceptable, it is not listed in the preferences.
"""


from employer import Employer
from applicant import Applicant


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


def do_matching(employers_file_path, applicants_file_path, verbose=True):
    """
    Does the matching
    """
    print(f"Verbose: {verbose}")
    print("Working with files: ", employers_file_path, applicants_file_path)
    employers = list_to_dict(get_people(employers_file_path, 'employer'))
    unmatched_employers = list(employers.keys())
    applicants = list_to_dict(get_people(applicants_file_path, 'applicant'))
    gale_shapley_stable_matching_algorithm(applicants, employers, unmatched_employers, verbose)
    print("Final Pairings are as follows:")
    print_pairings(employers, applicants)


def get_people(filename, type_):
    """
    Returns a list of (name,priority) pairs.
    """
    people = []
    with open(filename) as file:
        for line in file:
            pieces = line.split(':')
            name = pieces[0].strip()
            if name:
                priorities = pieces[1].strip().split(',')
                for i in range(len(priorities)):
                    priorities[i] = priorities[i].strip()
                if type_ == 'employer':
                    people.append(Employer(name, priorities))
                else:
                    people.append(Applicant(name, priorities))
    return people


def list_to_dict(people):
    """
    Converts a list of people objects to a dictionary where name -> person
    """
    dictionary = dict()
    for person in people:
        dictionary[person.name] = person
    return dictionary


def gale_shapley_stable_matching_algorithm(applicants, employers, unmatched_employers, verbose):
    """
    Does the bulk of the work of matching employers to applicants
    """
    while len(unmatched_employers) > 0:
        print("Unmatched employers ", unmatched_employers)
        applicant, employer = get_employer_and_applicant(applicants, employers, unmatched_employers, verbose)
        if applicant is None:
            continue
        if verbose:
            print(employer.name, 'proposes to', applicant.name)
        propose(applicant, employer, employers, unmatched_employers, verbose)
        if verbose:
            print("Tentative Pairings are as follows:")
            print_pairings(employers, applicants)


def get_employer_and_applicant(applicants, employers, unmatched_employers, verbose):
    """
    Returns this loops employer and applicant
    """
    employer = employers[unmatched_employers[0]]
    applicant_name = employer.next_proposal()
    if applicant_name is None:
        if verbose:
            print('No more options ' + str(employer))
        unmatched_employers.pop(0)
        applicant = None
        return applicant, employer
    applicant = applicants[applicant_name]
    return applicant, employer


def propose(applicant, employer, employers, unmatched_employers, verbose):
    """
    An employer proposes to an applicant
    """
    if applicant.evaluate_proposal(employer.name):
        if verbose:
            print('  ', applicant.name, 'accepts the proposal')
        update_partners(applicant, employer, employers, unmatched_employers)
    elif verbose:
        print('  ', applicant.name, 'rejects the proposal')


def update_partners(applicant, employer, employers, unmatched_employers):
    """
    Moves partners
    """
    if applicant.partner:
        old_employer = employers[applicant.partner]
        old_employer.partner = None
        old_employer.rank = 0
        unmatched_employers.append(old_employer.name)
    unmatched_employers.pop(0)
    applicant.partner = employer.name
    employer.partner = applicant.name
    employer.rank = employer.proposal_index


def print_pairings(employers, applicants):
    """
    Prints pairings between employers and applicants
    """
    employers_happiness = 0
    applicants_happiness = 0
    for employer in employers.values():
        if employer.partner:
            applicant = applicants[str(employer.partner)]
            employers_happiness += employer.rank
            applicants_happiness += applicant.rank
            print("\t", employer.name, employer.rank, 'is paired with', str(employer.partner), applicant.rank)
        else:
            print("\t", employer.name, 'is NOT paired')
    print("\tEmployees happiness: ", employers_happiness)
    print("\tApplicants happiness: ", applicants_happiness)


if __name__ == "__main__":
    main()