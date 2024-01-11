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

from src.employer import Employer
from src.applicant import Applicant

def get_people(filename, type_):
    """
    Returns a list of (name,priority) pairs.
    """
    people = []
    with open(filename) as f:
        for line in f:
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


def print_pairings(employers, applicants):
    """
    prints pairings between employers and applicants
    """
    for employer in employers.values():
        if employer.partner:
            print(employer.name, employer.rank, 'is paired with', str(employer.partner), applicants[str(employer.partner)].rank)
        else:
            print(employer.name, 'is NOT paired')


def list_to_dict(people):
    """
    converts a list of people objects to a dictionary where name -> person
    """
    dictionary = dict()
    for person in people:
        dictionary[person.name] = person
    return dictionary


def do_matching(employers_file_path, applicants_file_path, verbose=True):
    """
    does the matching
    """
    print(f"verbose: {verbose}")
    print("working with files: ", employers_file_path, applicants_file_path)

    employers = list_to_dict(get_people(employers_file_path, 'employer'))

    unmatched_employers = list(employers.keys())

    applicants = list_to_dict(get_people(applicants_file_path, 'applicant'))

    gale_shapley_stable_matching_algorithm(applicants, employers, unmatched_employers, verbose)


def get_employer_and_applicant(applicants, employers, unmatched_employers, verbose):
    """
    returns this loops employer and applicant
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


def gale_shapley_stable_matching_algorithm(applicants, employers, unmatched_employers, verbose):
    """
    does the bulk of the work of matching employers to applicants
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

    print("Final Pairings are as follows:")
    print_pairings(employers, applicants)


def propose(applicant, employer, employers, unmatched_employers, verbose):
    """
    An employer proposes to an applicant
    """
    if applicant.evaluate_proposal(employer.name):

        if verbose:
            print('  ', applicant.name, 'accepts the proposal')

        if applicant.partner:
            old_employer = employers[applicant.partner]
            old_employer.partner = None
            old_employer.rank = 0
            unmatched_employers.append(old_employer.name)

        unmatched_employers.pop(0)

        applicant.partner = employer.name
        employer.partner = applicant.name
        employer.rank = employer.proposal_index

    elif verbose:
        print('  ', applicant.name, 'rejects the proposal')


def main():
    do_matching("../data/Employers.txt", "../data/Applicants.txt")


if __name__ == "__main__":
    main()
