#ifndef INCLUDED_JBRANDOM
#define INCLUDED_JBRANDOM

#include <ctime>
#include <cstdlib>

class JBRandom  
{
public:
	JBRandom();
	~JBRandom();

  static int JBRandomize(int lowestNumber,int highestNumber)
  {
    if (!init)
    {
      init = true;
      srand(time(0));
    }

    int answer = (rand() % ((highestNumber + 1) - lowestNumber)) + lowestNumber;

    return answer;
  };

  static bool JBProbabilityPercent(short probability) // probability should be 1(%) to 100 (ie, percent)
  {
    if (!init)
    {
      init = true;
      srand(time(0));
    }

    if (rand() % 100 + 1 <= probability)
      return true;
    else
      return false;
  };

private:
  static bool init;

};

bool JBRandom::init = false;

#endif
