# https://dwarfstd.org/doc/DWARF4.pdf

const
  DW_ATE_address* = 0x1.cuint
  DW_ATE_boolean* = 0x2.cuint
  DW_ATE_complex_float* = 0x3.cuint
  DW_ATE_float* = 0x4.cuint
  DW_ATE_signed* = 0x5.cuint
  DW_ATE_signed_char* = 0x6.cuint
  DW_ATE_unsigned* = 0x7.cuint
  DW_ATE_unsigned_char* = 0x8.cuint
  DW_ATE_imaginary_float* = 0x09.cuint
  DW_ATE_packed_decimal* = 0x0a.cuint
  DW_ATE_numeric_string* = 0x0b.cuint
  DW_ATE_edited* = 0x0c.cuint
  DW_ATE_signed_fixed* = 0x0d.cuint
  DW_ATE_unsigned_fixed* = 0x0e.cuint
  DW_ATE_decimal_float* = 0x0f.cuint
  DW_ATE_UTF* = 0x10.cuint
  DW_ATE_lo_user* = 0x80.cuint
  DW_ATE_hi_user* = 0xFF.cuint

const
  DIFlagZero* = 0
  DIFlagPrivate* = 1
  DIFlagProtected* = 2
  DIFlagPublic* = 3
  DIFlagFwdDecl* = 1 shl 2
  DIFlagAppleBlock* = 1 shl 3
  DIFlagReservedBit4* = 1 shl 4
  DIFlagVirtual* = 1 shl 5
  DIFlagArtificial* = 1 shl 6
  DIFlagExplicit* = 1 shl 7
  DIFlagPrototyped* = 1 shl 8
  DIFlagObjcClassComplete* = 1 shl 9
  DIFlagObjectPointer* = 1 shl 10
  DIFlagVector* = 1 shl 11
  DIFlagStaticMember* = 1 shl 12
  DIFlagLValueReference* = 1 shl 13
  DIFlagRValueReference* = 1 shl 14
  DIFlagReserved* = 1 shl 15
  DIFlagSingleInheritance* = 1 shl 16
  DIFlagMultipleInheritance* = 2 shl 16
  DIFlagVirtualInheritance* = 3 shl 16
  DIFlagIntroducedVirtual* = 1 shl 18
  DIFlagBitField* = 1 shl 19
  DIFlagNoReturn* = 1 shl 20
  DIFlagTypePassByValue* = 1 shl 22
  DIFlagTypePassByReference* = 1 shl 23
  DIFlagEnumClass* = 1 shl 24
  DIFlagFixedEnum* = DIFlagEnumClass
  DIFlagThunk* = 1 shl 25
  DIFlagNonTrivial* = 1 shl 26
  DIFlagBigEndian* = 1 shl 27
  DIFlagLittleEndian* = 1 shl 28
  DIFlagIndirectVirtualBase* = (1 shl 2) or (1 shl 5)
  DIFlagAccessibility* = DIFlagPrivate or DIFlagProtected or DIFlagPublic
  DIFlagPtrToMemberRep* = DIFlagSingleInheritance or DIFlagMultipleInheritance or DIFlagVirtualInheritance
