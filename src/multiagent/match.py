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


from multiagent.employer import Employer
from multiagent.applicant import Applicant


def do_matching(employers_file_path, applicants_file_path, verbose=False):
    """
    Does the matching
    """
    if verbose:
        print("Working with files: ", employers_file_path, applicants_file_path)
    employers = list_to_dict(get_people(employers_file_path, 'employer'))
    unmatched_employers = list(employers.keys())
    applicants = list_to_dict(get_people(applicants_file_path, 'applicant'))
    gale_shapley_stable_matching_algorithm(applicants, employers, unmatched_employers, verbose)
    return get_pairings(employers, applicants, verbose)


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
        if verbose:
            print("Unmatched employers ", unmatched_employers)
        applicant, employer = get_employer_and_applicant(applicants, employers, unmatched_employers, verbose)
        if applicant is None:
            continue
        if verbose:
            print(employer.name, 'proposes to', applicant.name)
        propose(applicant, employer, employers, unmatched_employers, verbose)
        if verbose:
            print("Tentative Pairings are as follows:")
            overall, count = get_pairings(employers, applicants, verbose)
            print("Happiness:", overall)
            print("Count:", count)


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
        return None, None
    return applicants[applicant_name], employer


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


def get_pairings(employers, applicants, verbose):
    """
    Prints pairings between employers and applicants
    """
    print("Final Pairings are as follows:")
    employers_happiness = 0
    applicants_happiness = 0
    count = 0
    for employer in employers.values():
        if employer.partner:
            applicant = applicants[str(employer.partner)]
            employers_happiness += employer.rank
            applicants_happiness += applicant.rank
            count += 1
            print("\t", employer.name, employer.rank, 'is paired with', str(employer.partner), applicant.rank)
        else:
            print("\t", employer.name, 'is NOT paired')
    if verbose:
        print("\tEmployees happiness: ", employers_happiness)
        print("\tApplicants happiness: ", applicants_happiness)
    return employers_happiness + applicants_happiness, count

