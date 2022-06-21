#include <TROOT.h>
#include <iostream>

using namespace std;

extern "C" {
  void enewzsub_(int *z1, float *m1, float *bfr_ene, char matter1[33], 
  		 int *unit_pressure, float *pressure, float *temperature, 
  		 int *unit_thick, float *thick1, float *aft_ene);
}

int main() {
  Int_t z14 = 14; // Atomic number
  float m26si= 25.99232992; // Atomic mass
  
  Char_t mylar[34] = "mylar";
  
  Int_t unit_pressure = 1; // 1: Torr (not used for solid material)
  float pressure = 0; // in Torr (not used for solid material)
  float temperature = 299.; // in K (not used for solid material)
  Int_t unit_mm = 1; // 1: mm
  
  float th_mylar = 0.01; // thickness in mm

  Double_t Ebfr = 80.; // Initial energy in (MeV)

  float Ebfrpu, Eaftpu; // Energy per nucleon before and after E loss

  // Character definitions needed for enewz
  for(Int_t ichar=0;ichar<33;ichar++){ 
    if(mylar[ichar]=='\0'){
      mylar[ichar]=' ';
      mylar[ichar+1]='\0';
    }
  }

  Ebfrpu = Ebfr/m26si; 
  
  enewzsub_(&z14, &m26si, &Ebfrpu, mylar, &unit_pressure, &pressure, 
	    &temperature, &unit_mm, &th_mylar, &Eaftpu);
  
  cout << Ebfrpu*m26si << " -> " << Eaftpu*m26si << " MeV" << endl;
}
