// test code for the table model writer

#include "TableModel.h";

int main(int argc, char* argv[])
{

  FITS::setVerboseMode(true);

  TableModel test;

  test.ModelName = "Test";
  test.ModelUnits = " ";
  test.isRedshift = true;
  test.isAdditive = true;
  test.isError = false;

  test.Energies.resize(100);
  for (size_t i=0; i<100; i++) test.Energies[i] = 0.1+i*0.1;

  test.NumIntParams = 2;
  test.NumAddParams = 1;

  TableParameter testpar;

  testpar.Name = "param1";
  testpar.InterpolationMethod = 0;
  testpar.InitialValue = 1.0;
  testpar.Delta = 0.1;
  testpar.Minimum = 0.0;
  testpar.Bottom = 0.0;
  testpar.Top = 2.0;
  testpar.Maximum = 2.0;

  testpar.TabulatedValues.resize(11);
  for (size_t i=0; i<11; i++) testpar.TabulatedValues[i] = 0.2*i;

  test.Parameters.push_back(testpar);

  testpar.Name = "param2";
  testpar.InterpolationMethod = 0;
  testpar.InitialValue = 5.0;
  testpar.Delta = 0.1;
  testpar.Minimum = 4.6;
  testpar.Bottom = 4.6;
  testpar.Top = 5.4;
  testpar.Maximum = 5.4;

  testpar.TabulatedValues.resize(5);
  for (size_t i=0; i<5; i++) testpar.TabulatedValues[i] = 4.6+0.2*i;

  test.Parameters.push_back(testpar);

  testpar.Name = "addparam";
  testpar.InterpolationMethod = 0;
  testpar.InitialValue = 0.0;
  testpar.Delta = 0.1;
  testpar.Minimum = 0.0;
  testpar.Bottom = 0.0;
  testpar.Top = 5.0;
  testpar.Maximum = 5.0;
  testpar.TabulatedValues.resize(0);

  test.Parameters.push_back(testpar);

  TableSpectrum testspec;

  testspec.Flux.resize(99);
  testspec.ParameterValues.resize(2);

  RealArray addFlux(99);

  for (size_t i1=0; i1<11; i1++) {
    for (size_t i2=0; i2<5; i2++) {
      testspec.ParameterValues[0] = 0.2*i1;
      testspec.ParameterValues[1] = 4.6+0.2*i2;
      for (size_t j=0; j<99; j++) {
	testspec.Flux[j] = testspec.ParameterValues[0]+10*testspec.ParameterValues[1];
	addFlux[j] = 1.0*(i1+1)*(i2+1);
      }
      testspec.addFlux.push_back(addFlux);
      test.Spectra.push_back(testspec);
      testspec.addFlux.clear();
    }
  }

  test.write("test.mod");

  return(0);
}
