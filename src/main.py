from schelling import Schelling
from os import mkdir, path


def main():
    # This is where data will be stored in the /data folder.
    # Change if you want to save new data.
    # Don't change if you want a run to be overwritten.
    run = set_up_simulation(4)
    print("Running simulation 1...")
    segregation_1 = Schelling(50, 50, 0.3, [0.3, 0.3], 2, True)
    segregation_1.simulate(run, 1, 200)
    print()
    print("Running simulation 2...")
    segregation_2 = Schelling(50, 50, 0.3, [0.5, 0.5], 2, True)
    segregation_2.simulate(run, 2, 200)
    print()
    print("Running simulation 3...")
    segregation_3 = Schelling(50, 50, 0.3, [0.8, 0.8], 2, True)
    segregation_3.simulate(run, 3, 200)
    print()


def set_up_simulation(run):
    run_path = path.join("../data/", f"run{run}")
    try:
        mkdir(run_path)
    except FileExistsError:
        pass
    return run


if __name__ == "__main__":
    main()
