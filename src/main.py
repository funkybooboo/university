from schelling import Schelling


def main():
    # This is where data will be stored in the /data folder.
    # Change if you want to save new data.
    # Don't change if you want a run to be overwritten.
    run = 8
    print("Running given simulations...")
    print()
    print("Running simulation 1...")
    segregation_1 = Schelling(50, 50, 0.3, [[0.3, 0], [0, 0.3]], 2, True)
    segregation_1.simulate(run, 1, 200)
    print()
    print("Running simulation 2...")
    segregation_2 = Schelling(50, 50, 0.3, [[0.5, 0], [0, 0.5]], 2, True)
    segregation_2.simulate(run, 2, 200)
    print()
    print("Running simulation 3...")
    segregation_3 = Schelling(50, 50, 0.3, [[0.8, 0], [0, 0.8]], 2, True)
    segregation_3.simulate(run, 3, 200)
    print()
    print()
    print("Running my simulations...")
    print()
    print("Running simulation 4...")
    similarity_thresholds = [[0]]
    segregation_4 = Schelling(50, 50, 0.5, similarity_thresholds, 1, True)
    segregation_4.simulate(run, 4, 200)
    print()
    print("Running simulation 5...")
    similarity_thresholds = [[0.5, 0.5], [0.5, 0.5]]
    segregation_5 = Schelling(50, 50, 0.5, similarity_thresholds, 2, True)
    segregation_5.simulate(run, 5, 200)
    print()
    print("Running simulation 6...")
    similarity_thresholds = [[0.3, 0.1, 0], [0.1, 0.5, 0.2], [0, 0.2, 0.3]]
    segregation_6 = Schelling(50, 50, 0.4, similarity_thresholds, 3, True)
    segregation_6.simulate(run, 6, 200)
    print()
    print("Running simulation 7...")
    similarity_thresholds = [[0, 0.3, 0.2, 0.2], [0.3, 0.7, 0, 0], [0.3, 0, 0.3, 0], [0.2, 0, 0, 0.2]]
    segregation_7 = Schelling(50, 50, 0.3, similarity_thresholds, 4, True)
    segregation_7.simulate(run, 7, 200)
    print()
    print("Running simulation 8...")
    similarity_thresholds = [[0.1, 0.1, 0.1, 0.1, 0.1], [0.1, 0.1, 0.1, 0.1, 0.1], [0.1, 0.1, 0.1, 0.1, 0.1], [0.1, 0.1, 0.1, 0.1, 0.1], [0.1, 0.1, 0.1, 0.1, 0.1]]
    segregation_8 = Schelling(50, 50, 0.3, similarity_thresholds, 5, True)
    segregation_8.simulate(run, 8, 200)
    print()
    print("Running simulation 9...")
    similarity_thresholds = [[0.5, 0, 0, 0, 0, 0], [0.1, 0.5, 0.1, 0, 0.2, 0], [0.1, 0.5, 0, 0.1, 0, 0.1], [0, 0.1, 0.2, 0.5, 0, 0.1], [0.2, 0, 0, 0, 0.5, 0.2], [0, 0, 0.2, 0, 0.2, 0.5]]
    segregation_9 = Schelling(50, 50, 0.4, similarity_thresholds, 6, True)
    segregation_9.simulate(run, 9, 200)
    print()
    print("Running simulation 10...")
    similarity_thresholds = [
        [0.7, 0, 0.2, 0, 0, 0, 0], [0, 0.6, 0.2, 0, 0, 0, 0], [0, 0, 0.2, 0, 0, 0.7, 0], [0, 0.4, 0, 0.4, 0, 0, 0], [0, 0, 0, 0.3, 0.3, 0.3, 0], [0.1, 0, 0, 0, 0, 0.6, 0], [0.1, 0.1, 0, 0, 0, 0, 0.4]
    ]
    segregation_10 = Schelling(50, 50, 0.5, similarity_thresholds, 7, True)
    segregation_10.simulate(run, 10, 200)
    print()
    print()
    print("Running three random simulations...")
    print()
    print("Running simulation 11...")
    Schelling.run_random_simulation(run, 11, True)
    print()
    print("Running simulation 12...")
    Schelling.run_random_simulation(run, 12, True)
    print()
    print("Running simulation 13...")
    Schelling.run_random_simulation(run, 13, True)


if __name__ == "__main__":
    main()
