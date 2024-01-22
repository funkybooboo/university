from segregation import Segregation
from os import mkdir, path


def main():
    # This is where data will be stored in the /data folder.
    # Change if you want to save new data.
    # Don't change if you want a run to be overwritten.
    run = 5
    run_path = path.join("../data/", f"run{run}")
    try:
        mkdir(run_path)
    except FileExistsError:
        pass

    ##Starter Simulation
    schelling_0 = Segregation(5, 5, 0.3, 0.3, 200, run, 2, True)
    schelling_0.populate()

    ##First Simulation
    schelling_1 = Segregation(50, 50, 0.3, 0.3, 200, run, 2, True)
    schelling_1.populate()

    schelling_2 = Segregation(50, 50, 0.3, 0.5, 200, run, 2, True)
    schelling_2.populate()

    schelling_3 = Segregation(50, 50, 0.3, 0.8, 200, run, 2, True)
    schelling_3.populate()

    schelling_1.plot('Schelling Model with 2 colors: Initial State', 'schelling_2_initial.png')

    schelling_0.move_locations()
    schelling_1.move_locations()
    schelling_2.move_locations()
    schelling_3.move_locations()
    schelling_0.plot('Schelling Model with 2 colors: Final State with Happiness Threshold 30%',
                     'schelling_0_30_final.png')
    schelling_1.plot('Schelling Model with 2 colors: Final State with Happiness Threshold 30%',
                     'schelling_30_final.png')
    schelling_2.plot('Schelling Model with 2 colors: Final State with Happiness Threshold 50%',
                     'schelling_50_final.png')
    schelling_3.plot('Schelling Model with 2 colors: Final State with Happiness Threshold 80%',
                     'schelling_80_final.png')

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


if __name__ == "__main__":
    main()