#if defined _vexdum_const_included
  #endinput
#endif
#define _vexdum_const_included

// TraceLine Integer
enum {
  TR_INT_fAllSolid,     // if true, plane is not valid
  TR_INT_fStartSolid,     // if true, the initial point was in a solid area
  TR_INT_fInOpen,
  TR_INT_fInWater,
  TR_INT_iHitgroup,     // 0 == generic, non zero is specific body part
};

// TraceLine Float
enum {
  TR_FL_flFraction,     // time completed, 1.0 = didn't hit anything
  TR_FL_flPlaneDist,
};

// TraceLine Vector
enum {
  TR_VEC_vecEndPos,     // final position
  TR_VEC_vecPlaneNormal,  // surface normal at impact
};

// TraceLine Edict
enum {
  TR_ENT_pHit,      // entity the surface is on
};
