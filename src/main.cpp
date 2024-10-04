#include "sortutils.hpp"

#include <iostream>

int main()
{

}

template <typename T>
std::string getCollectionStats(std::string& collectionTitle, T& collection)
{
    std::string stats = "--- " + collectionTitle + " Performance ---";

    // TODO add Sequential to stats

    // TODO add Parallel to stats

    return stats;
}
