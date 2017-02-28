#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* replaces internal workings of "fastgen" (and therefore also
   "given_RADecMJD_return_SunAngle")from Tcl version with a faster one (in C) */

#define DEG2RAD 0.017453292519943295769
#define RAD2DEG 57.295779513082320877
#define EPS 1.e-12
#define PI 3.1415926535897932385


void sunPos(double MJD, double SunVect[]) {
  double t, m, r, l;

  t = MJD - 4.5e4;
  m = t * .985600267 + 27.26464;
  m = fmod(m,360);
  m = m * DEG2RAD;
  r = 1.00014 - cos(m) * .01672 - sin(m * 2) * 1.4e-4;
  l = t * .985609104 + 309.44862 + sin(m) * 1.91553 + sin(m * 2) * .0201;
  l = fmod(l,360);
  l = l * DEG2RAD;

  SunVect[0] = r * cos(l);
  SunVect[1] = r * .91744 * sin(l);
  SunVect[2] = r * .39788 * sin(l);

}

void polToVect(double PVect[], double Vect[]) {
  double R, Lat, Lon;

  R = PVect[0];
  Lon = PVect[1];
  Lat = PVect[2];

  Vect[0] = R * cos(Lat) * cos(Lon);
  Vect[1] = R * cos(Lat) * sin(Lon);
  Vect[2] = R * sin(Lat);

}

void rotVect(double RM[3][3], double Vect[], double RVect[]) {
  double y0, y1, y2;
  y0 = RM[0][0] * Vect[0] + RM[0][1] * Vect[1] + RM[0][2] * Vect[2];
  y1 = RM[1][0] * Vect[0] + RM[1][1] * Vect[1] + RM[1][2] * Vect[2];
  y2 = RM[2][0] * Vect[0] + RM[2][1] * Vect[1] + RM[2][2] * Vect[2];
  RVect[0] = y0;
  RVect[1] = y1;
  RVect[2] = y2;
}

void eulerToRM(double EA[],double RM[3][3]) {
  /* useful for viewing */

  double phi, psi, theta;
  double cos_phi, cos_theta, cos_psi, sin_phi, sin_theta, sin_psi;

  phi = EA[0];
  psi = EA[1];
  theta = EA[2];

  cos_phi = cos(phi);
  cos_theta = cos(theta);
  cos_psi = cos(psi);
  sin_phi = sin(phi);
  sin_theta = sin(theta);
  sin_psi = sin(psi);

  RM[0][0] =   cos_psi * cos_theta * cos_phi -  sin_psi * sin_phi;
  RM[0][1] =   cos_psi * cos_theta * sin_phi +  sin_psi * cos_phi;
  RM[0][2] =  0.0         -  cos_psi * sin_theta;
  RM[1][0] = 0.0- sin_psi * cos_theta * cos_phi -  cos_psi * sin_phi;
  RM[1][1] = 0.0- sin_psi * cos_theta * sin_phi +  cos_psi * cos_phi;
  RM[1][2] =                 sin_psi * sin_theta;
  RM[2][0] =   sin_theta * cos_phi;
  RM[2][1] =   sin_theta * sin_phi;
  RM[2][2] =   cos_theta;

}

void eulerToRM_righthand(double EA[],double RM[3][3]) {
  /* useful for rolls */

  double phi, psi, theta;
  double cos_phi, cos_theta, cos_psi, sin_phi, sin_theta, sin_psi;

  phi = EA[0];
  psi = EA[1];
  theta = EA[2];

  cos_phi = cos(phi);
  cos_theta = cos(theta);
  cos_psi = cos(psi);
  sin_phi = sin(phi);
  sin_theta = sin(theta);
  sin_psi = sin(psi);

  RM[0][0] =  cos_psi*sin_phi - cos_phi*cos_theta*sin_psi;
  RM[1][0] = -cos_psi*cos_phi - sin_phi*cos_theta*sin_psi;
  RM[2][0] =  sin_psi*sin_theta;
  RM[0][1] = -sin_psi*sin_phi - cos_phi*cos_theta*cos_psi;
  RM[1][1] =  sin_psi*cos_phi - sin_phi*cos_theta*cos_psi;
  RM[2][1] =  cos_psi*sin_theta;
  RM[0][2] = -cos_phi*sin_theta;
  RM[1][2] = -sin_phi*sin_theta;
  RM[2][2] = -cos_theta;

}

void precessEuler(double Epoch, double MJD, double EA[3]) {
  double u0, u, phi, psi, theta;

  u0 = (Epoch - 15020.) / 36524.22;
  u = (MJD - Epoch) / 36524.22;
  phi =  -(u0*1.396 + 2304.25 + (u*.018 + .302) *u) * u * DEG2RAD /3600.;
  psi =  phi - u * .791 * u * DEG2RAD / 3600.;
  theta = (2004.682 - u0*.853 + (-.426 - u*.042)* u)* u* DEG2RAD /3600.;

  EA[0] = phi;
  EA[1] = psi;
  EA[2] = theta;
}

void precess(double Epoch, double MJD, double Vect[], double RVect[]) {
  double EA[3],RM[3][3];

  precessEuler(Epoch, MJD, EA);
  eulerToRM(EA,RM);
  rotVect(RM,Vect,RVect);

}

void invRotMat(double RM[3][3], double InvRM[3][3]) {
  InvRM[0][0] = RM[0][0];
  InvRM[0][1] = RM[1][0];
  InvRM[0][2] = RM[2][0];
  InvRM[1][0] = RM[0][1];
  InvRM[1][1] = RM[1][1];
  InvRM[1][2] = RM[2][1];
  InvRM[2][0] = RM[0][2];
  InvRM[2][1] = RM[1][2];
  InvRM[2][2] = RM[2][2];
}

int vectToPol(double Vect[],double VRet[]) {
  double norm01, R, lon, lat, c, s;
  double norm(double[]);

  norm01 = Vect[0]*Vect[0]+Vect[1]*Vect[1];
  R= norm(Vect);
  lon = 0.0;
  lat = 0.0;
  if (R == 0.0) {
    /* eek, failure */
    VRet[0] = 0.0;
    VRet[1] = 0.0;
    VRet[2] = 0.0;
    return -1;
  }

  if (norm01 == 0.0) {
    /* we're pointing straight up */
    VRet[0] = R;
    VRet[1] = 0.0;
    VRet[2] = 90 * DEG2RAD;
    return -1;
  }

  norm01 = sqrt(norm01);
  lat = asin(Vect[2]/R);
  c = Vect[0]/norm01;
  s = Vect[1]/norm01;
  
  if (norm01 < EPS) {
    lon = 0.0;
  } else if ( fabs(s) < EPS) {
    lon = (1.0 - c/fabs(c)) * PI/2.0;
  } else {
    lon = atan((1.0-c)/s) * 2.0;
  }

  while (lon >= 2.0*PI) {
    lon = lon - 2*PI;
  }
  while (lon < 0) {
    lon = lon + 2*PI;
  }

  VRet[0] = R;
  VRet[1] = lon;
  VRet[2] = lat;

  return 0;
}

int rotPVect(double Ang[3], double Vect[3], double VRet[3]) {
  double RM[3][3], U[3], V[3];
  int ireturn;

  eulerToRM(Ang,RM);
  polToVect(Vect,U);
  rotVect(RM,U,V);

  /* note dual behavior-- we're setting VRet implicitly to return back
     to the calling program, while also setting a return flag (ireturn)
     explicitly to also return to the calling program. */

  ireturn = vectToPol(V,VRet);

  return ireturn;

}

void setEuler(double V1[3], double V2[3], double Euler[3]) {
  double phi,psi, theta;
  double TempV[3];
  int ireturn;

  phi = V1[1];
  psi = 0.0;
  theta = PI/2.0 - V1[2];

  Euler[0] = phi;
  Euler[1] = psi;
  Euler[2] = theta;

  /* note we only seem to do this to check the return value, as we've already
     set our return vector (save for a final tweaking of its psi value) */

  ireturn = rotPVect(Euler, V2, TempV);

  if (ireturn != 0) {
    printf("Warning, problem with euler angle rotation, continuing\n");
  } else {
    /* replace our psi angle with rotated longitude */
    Euler[1] = TempV[1] - PI/2.0;
  }

}

double norm(double Vect[]) {
  double sum;

  sum = sqrt(Vect[0]*Vect[0]+Vect[1]*Vect[1]+Vect[2]*Vect[2]);
  return sum;

}

double angDistance(double Vect1[], double Vect2[]) {
  double d1, r;

  d1 = Vect1[0]*Vect2[0] + Vect1[1]*Vect2[1] + Vect1[2]*Vect2[2];

  d1 = d1/(norm(Vect1)*norm(Vect2));

  if (d1 > 1.0-EPS)
    d1 = 1.0;
  if (d1 < -1.0+EPS)
    d1 = -1.0;

  r = acos(d1);

  if (r < 0.0 || r > PI)
    r = -1.0;

  return r;

}


double c_given_RADecMJD_return_SunAngle(double RA, double Dec, double MJD) {

  double SunAngle;
  double SunVect[3], TargetVect[3], TargetPolarVect[3], NewTVect[3];
  double Euler[3], RM[3][3], InvRM[3][3], FOV[3], RotatedTargetVect[3];

  sunPos(MJD,SunVect);

  TargetPolarVect[0] = 1.0;
  TargetPolarVect[1] = RA * DEG2RAD;
  TargetPolarVect[2] = Dec * DEG2RAD;

  polToVect(TargetPolarVect,TargetVect);

  precess(51544.5000,MJD,TargetVect,NewTVect); /* precess to J2000 */
  vectToPol(NewTVect,TargetPolarVect);

  FOV[0] = 0.0; FOV[1] = 0.0; FOV[2] = 1.0;

  setEuler(TargetPolarVect,SunVect,Euler);

  eulerToRM(Euler,RM);
  invRotMat(RM, InvRM);

  rotVect(InvRM,FOV,RotatedTargetVect);

  /* is this TargetVect or NewTVect? */
  SunAngle = angDistance(SunVect,RotatedTargetVect);

  SunAngle = SunAngle*RAD2DEG;

  return SunAngle;

}


void fastRADecMJD2Roll(double RA, double Dec, double MJD,
				    double SunMin, double SunMax,
				    double SunLimit, double ReturnVals[4]) {

  double SunAngle;
  double SunVect[3], TargetVect[3], TargetPolarVect[3], NewTVect[3];
  double N[3], W[3], X[3], Y[3];
  double YNorth, YWest, Roll, Ratio, MinRoll, MaxRoll;
  void vectProd(double[],double[],double[]), normVect(double[]);

  sunPos(MJD,SunVect);

  TargetPolarVect[0] = 1.0;
  TargetPolarVect[1] = RA * DEG2RAD;
  TargetPolarVect[2] = Dec * DEG2RAD;

  polToVect(TargetPolarVect,TargetVect);

  /* Hey, wait, do we actually use this at all? */
  precess(51544.5000,MJD,TargetVect,NewTVect); /* precess to J2000 */
  TargetVect[0] = NewTVect[0];
  TargetVect[1] = NewTVect[1];
  TargetVect[2] = NewTVect[2];
  /* vectToPol(NewTVect,TargetPolarVect); is not needed here */

  SunAngle = c_given_RADecMJD_return_SunAngle(RA,Dec,MJD);

  if (SunAngle >= SunMin && SunAngle <= SunMax) {
    ;
  } else {
    /* printf("Bad sun angle: %f %f %f\n",SunAngle,SunMin,SunMax); */
    ReturnVals[0] = 0; ReturnVals[1] = 0; ReturnVals[2] = 0;
    ReturnVals[3] = -1;
    return;
  }

  SunAngle = SunAngle * DEG2RAD;
  SunLimit = SunLimit * DEG2RAD;

  N[0] = 0.0; N[1] = 0.0; N[2] = 1.0;

  vectProd(TargetVect,N, W);
  normVect(W);
  vectProd(W,TargetVect,N);
  normVect(N);

  if (N[2] < 0.0) {
    /* north became south, correcting */
    N[0] = 0.0-N[0];
    N[1] = 0.0-N[1];
    N[2] = 0.0-N[2];
  }

  vectProd(TargetVect,SunVect,X);
  normVect(X);
  vectProd(X,TargetVect,Y);
  normVect(Y);

  YNorth = angDistance(Y,N);
  YWest = angDistance(Y,W);

  if (YNorth <= 90.0*DEG2RAD) {
    Roll = YWest;
  } else {
    Roll = (360.0*DEG2RAD) - YWest;
  }

  Ratio = cos(SunLimit)/sin(SunAngle);
  if (Ratio > 1.0) Ratio = 1.0;
  if (Ratio < -1.0) Ratio = -1.0;

  Ratio = acos(Ratio);

  MinRoll = (Roll-Ratio)*RAD2DEG;
  MaxRoll = (Roll+Ratio)*RAD2DEG;
  Roll = Roll*RAD2DEG;

  if (MinRoll < 0.0) MinRoll = 360.0 + MinRoll;
  if (MaxRoll > 360.0) MaxRoll = MaxRoll - 360.0;

  ReturnVals[0] = MinRoll; ReturnVals[1] = MaxRoll; ReturnVals[2] = Roll;
  ReturnVals[3] = 0;

  return;

}

void vectProd(double One[], double Two[], double Result[]) {

  Result[0] = One[1] * Two[2] - One[2] * Two[1];
  Result[1] = One[2] * Two[0] - One[0] * Two[2];
  Result[2] = One[0] * Two[1] - One[1] * Two[0];

}

void normVect(double Vect[]) {
  double NV, norm(double[]);

  NV = norm(Vect);
  if (NV == 0) {
    Vect[0] = 0; Vect[1] = 0; Vect[2] = 0;
  } else {
    Vect[0] = Vect[0]/NV;
    Vect[1] = Vect[1]/NV;
    Vect[2] = Vect[2]/NV;
  }

}

