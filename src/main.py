from schelling import Schelling
from os import mkdir, path


def main():
    # This is where data will be stored in the /data folder.
    # Change if you want to save new data.
    # Don't change if you want a run to be overwritten.
    run = set_up_simulation(6)
    
    segregation_1 = Schelling(50, 50, 0.3, 0.3, 200, 2, run, True)
    segregation_1.simulate()

    segregation_2 = Schelling(50, 50, 0.3, 0.5, 200, 2, run, True)
    segregation_2.simulate()

    segregation_3 = Schelling(50, 50, 0.3, 0.8, 200, 2, run, True)
    segregation_3.simulate()

    #Second Simulation Measuring Segregation
    # similarity_threshold_ratio = {}
    # for i in [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7]:
    #     schelling = Segregation(50, 50, 0.3, i, 500, 2)
    #     schelling.populate()
    #     schelling.update()
    #     similarity_threshold_ratio[i] = schelling.calculate_similarity()
    #
    # fig, ax = plt.subplots()
    # plt.plot(similarity_threshold_ratio.keys(), similarity_threshold_ratio.values(), 'ro')
    # ax.set_title('Similarity Threshold vs. Mean Similarity Ratio', fontsize=15, fontweight='bold')
    # ax.set_xlim([0, 1])
    # ax.set_ylim([0, 1.1])
    # ax.set_xlabel("Similarity Threshold")
    # ax.set_ylabel("Mean Similarity Ratio")
    # plt.savefig('schelling_segregation.png')


def set_up_simulation(run):
    run_path = path.join("../data/", f"run{run}")
    try:
        mkdir(run_path)
    except FileExistsError:
        pass
    return run


if __name__ == "__main__":
    main()