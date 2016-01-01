#include <iostream>
#include <fstream>

#define IX(arr,i,j) ((arr)[i+100*j])

using namespace std;

int a[100][100] = {0};
int b[100][100] = {0};


int neighbors(int* arr, int x, int y) {
  int acc = -IX(arr,x,y);
  for (int i = max(x-1,0); i < min(x+2,100); i++) {
    for (int j = max(y-1,0); j < min(y+2,100); j++) {
      acc = acc + IX(arr,i,j);
    }
  }

  return acc;
}

void initialize() {
  ifstream myfile;
  myfile.open("input");
  string l;
  int* ptr = &a[0][0];

  for (int y = 0; y < 100; y++) {
    myfile >> l;
    // cout << l << endl;
    for (int x = 0; x < 100; x++) {
      IX(ptr,x,y) = l[x] == '#';
    }
  }

  myfile.close();
}

void printArr(int* arr) {
  for (int y = 0; y < 100; y++) {
    for (int x = 0; x < 100; x++) {
      cout << ((IX(arr,x,y) == 1) ? '#' : '.');
    }
    cout << endl;
  }
}

void setCorners(int* arr) {
  IX(arr,0,0) = IX(arr,0,99) = IX(arr,99,0) = IX(arr,99,99) = 1;
}

void partX(bool corners) {
  initialize();

  int* aa = &a[0][0];
  int* bb = &b[0][0];

  for (int i=0; i < 100; i++) {
    if (corners) setCorners(aa);
    for (int y=0; y < 100; y++) {
      for (int x=0; x < 100; x++) {
        int ns = neighbors(aa, x, y);
        if (IX(aa,x,y) == 1) { // on
          if (ns == 2 || ns == 3)
            IX(bb,x,y) = 1;
          else 
            IX(bb,x,y) = 0;
        }
        else {
          if (ns == 3)
            IX(bb,x,y) = 1;
          else 
            IX(bb,x,y) = 0;
        }
      }
    }
    int* c = aa; 
    aa = bb; bb = c;
  }
  // now count them up
  if (corners) setCorners(aa);
  int acc = 0;
  for(int i=0; i < 100*100; i++) {
    acc += aa[i];
  }
  cout << acc << endl;
}

int main() {
  cout << "Day 18: Like a GIF For Your Yard" << endl;

  partX(false);
  partX(true);

  return 0;
}

