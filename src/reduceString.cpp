#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::string reduceString(CharacterVector& x)
{
    size_t nbRepeats = 0;
    std::string r = "";
    std::string lastStr = "";
    for (size_t i = 0 ; i < x.size() ; ++i)
    {
        std::istringstream iss(Rcpp::as<std::string>(x[i]));
        std::string str;

        while (iss >> str)
        {
            if (str == lastStr)
            {
                ++nbRepeats;
            } else
            {
                if (nbRepeats > 0)
                {
                    if (nbRepeats == 1)
                    {
                       r += lastStr + " "; 
                    } else if (nbRepeats == 2)
                    {
                       r += lastStr + " " + lastStr + " "; 
                    } else
                    {
                        r += "REP " + lastStr + " " + std::to_string(nbRepeats) + " ";
                    }
                }

                nbRepeats = 1;
                lastStr = str;
            }
        }
    }

    if (nbRepeats == 1)
    {
       r += lastStr; 
    } else if (nbRepeats == 2)
    {
       r += lastStr + " " + lastStr; 
    } else
    {
        r += "REP " + lastStr + " " + std::to_string(nbRepeats);
    }

    return r;
}
