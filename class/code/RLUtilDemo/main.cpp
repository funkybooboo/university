#include "rlutil.h" // Reference: https://github.com/tapio/rlutil

//
// This is a simple demonstration program used to test out the capabilities
// of the rultil.h code.
//
int main()
{
    rlutil::saveDefaultColor();

    rlutil::cls();
    rlutil::hidecursor();

    std::cout << "Move the object using the arrow keys";

    int x = 1;
    int y = 2;
    rlutil::locate(x, y);
    std::cout << '@';
    for (bool done = false; !done;)
    {
        if (kbhit())
        {
            // Remove previous location
            rlutil::locate(x, y);
            std::cout << " ";

            int input = rlutil::getkey();
            switch (input)
            {
                case rlutil::KEY_LEFT:
                    x--;
                    break;
                case rlutil::KEY_RIGHT:
                    x++;
                    break;
                case rlutil::KEY_UP:
                    y--;
                    break;
                case rlutil::KEY_DOWN:
                    y++;
                    break;
                case rlutil::KEY_ESCAPE:
                    done = true;
            }
            // Display at new position
            rlutil::locate(x, y);
            std::cout << '@';
            std::cout.flush();
        }
    }

    rlutil::showcursor();
    rlutil::resetColor();

    return 0;
}
