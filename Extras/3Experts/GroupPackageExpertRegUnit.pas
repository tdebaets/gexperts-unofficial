unit GroupPackageExpertRegUnit;

interface

resourcestring
  sParentEntryName = 'GroupPackageExpert';
  sParentEntryCaption = '&Daniel''s Experts';

procedure Register;

implementation

uses ExptIntf, GroupPackageExpertUnit, SpaceByPixelExpertUnit, FieldByNameExpert,
     CompNameListExpert, InsertMenuItemUnit,
     ToolIntf, EditIntf, windows;

procedure Register;
begin
  RegisterLibraryExpert(TGroupPackageExpert.Create); //Must be loaded first
  RegisterLibraryExpert(TSpaceByPixelExpert.Create);
  RegisterLibraryExpert(TCompNameListExpert.Create);
  RegisterLibraryExpert(TFieldByNameExpert.Create);
end;

end.
