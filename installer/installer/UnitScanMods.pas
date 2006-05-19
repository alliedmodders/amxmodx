unit UnitScanMods;

interface

uses SysUtils, Classes, Dialogs;

function GetAllMods(eBaseDir: String; eSearchNames: Boolean): TStringList;
function GetModPathName(eMod: String): String;
function ModIndex(Name: String; CheckName: Boolean): Integer;

var eMods: TStringList;
    eModsPath: TStringList;

implementation

uses UnitFunctions;

// functions

function GetAllMods(eBaseDir: String; eSearchNames: Boolean): TStringList;
var i: integer;
    eDirectories: TStringList;
begin
  eBaseDir := IncludeTrailingPathDelimiter(eBaseDir);
  eDirectories := GetAllFiles(eBaseDir + '*.*', faDirectory, False, True, False);

  if eDirectories.Count <> 0 then begin
    for i := eDirectories.Count -1 downto 0 do begin
      if (ModIndex(eDirectories[i], True) = -1) then
        eDirectories.Delete(i)
      else
        eDirectories[i] := eMods[ModIndex(eDirectories[i], True)];
    end;
    Result := eDirectories;
  end
  else
    Result := TStringList.Create; // bad but prevents exception!
end;

function GetModPathName(eMod: String): String;
var i: integer;
begin
  Result := '';
  for i := 0 to eMods.Count -1 do begin
    if LowerCase(eMod) = LowerCase(eMods[i]) then begin
      Result := eModsPath[i];
      break;
    end;
  end;
end;

function ModIndex(Name: String; CheckName: Boolean): Integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to eMods.Count -1 do begin
    if (LowerCase(eModsPath[i]) = LowerCase(Name)) then begin
      Result := i;
      break;
    end
    else if (LowerCase(eMods[i]) = LowerCase(Name)) and (CheckName) then begin
      Result := i;
      break;
    end;
  end;
end;

// misc...

initialization

// Create objects on start
  eMods := TStringList.Create;
  eModsPath := TStringList.Create;
// Add mods ...
  eMods.Add('Action Half-Life');
  eMods.Add('Adrenaline Gamer Steam');
  eMods.Add('Adrenalinegamer 3.x');
  eMods.Add('Adrenalinegamer 4.x');
  eMods.Add('Arg!');
  eMods.Add('Azure Sheep');
  eMods.Add('The Battle Grounds');
  eMods.Add('Bot');
  eMods.Add('Bumper Cars');
  eMods.Add('BuzzyBots');
  eMods.Add('Counter-Strike 1.3');
  eMods.Add('Counter-Strike');
  eMods.Add('CS 1.5 for Steam');
  eMods.Add('Condition Zero');
  eMods.Add('Desert Crisis');
  eMods.Add('Deathmatch Classic');
  eMods.Add('Day of Defeat');
  eMods.Add('Digital Paintball');
  eMods.Add('Dragon Mod Z');
  eMods.Add('Earth''s Special Forces');
  eMods.Add('Existence');
  eMods.Add('Firearms');
  eMods.Add('Retro Firearms');
  eMods.Add('Freeze');
  eMods.Add('Frontline Force');
  eMods.Add('GangstaWars');
  eMods.Add('Gangwars');
  eMods.Add('Opposing Force');
  eMods.Add('Global Warfare');
  eMods.Add('Goldeneye');
  eMods.Add('HL-Rally');
  eMods.Add('Holy Wars');
  eMods.Add('Hostile Intent');
  eMods.Add('International Online Soccer');
  eMods.Add('Judgement');
  eMods.Add('Kanonball');
  eMods.Add('Monkeystrike');
  eMods.Add('Morbid Inclination');
  eMods.Add('Natural Selection');
  eMods.Add('Natural Selection Beta');
  eMods.Add('OeL Half-Life');
  eMods.Add('Over Ground');
  eMods.Add('Outlawsmod');
  eMods.Add('Operations 1942');
  eMods.Add('Open-Source Jailbreak');
  eMods.Add('Out Break');
  eMods.Add('Oz Deathmatch');
  eMods.Add('Paintball');
  eMods.Add('Public Enemy');
  eMods.Add('Phineas Bot');
  eMods.Add('Point of No Return');
  eMods.Add('Pirates, Vikings and Knights');
  eMods.Add('Rocket Crowbar 2');
  eMods.Add('Retro Counter-Strike');
  eMods.Add('Gunman Chronicles');
  eMods.Add('Ricochet');
  eMods.Add('Rocket Crowbar');
  eMods.Add('Rival Species');
  eMods.Add('Scientist Hunt');
  eMods.Add('The Ship');
  eMods.Add('Science & Industry');
  eMods.Add('Snow-War');
  eMods.Add('StargateTC');
  eMods.Add('Sven Coop');
  eMods.Add('Swarm');
  eMods.Add('Team Fortress Classic');
  eMods.Add('The Wastes');
  eMods.Add('Project Timeless');
  eMods.Add('Tour of Duty');
  eMods.Add('Train Hunters');
  eMods.Add('The Terrorist Revenge');
  eMods.Add('The Specialists');
  eMods.Add('The Specialists');
  eMods.Add('The Trenches');
  eMods.Add('Underworld Bloodline');
  eMods.Add('Half-Life Deathmatch');
  eMods.Add('VampireSlayer');
  eMods.Add('Wanted!');
  eMods.Add('Wasteland');
  eMods.Add('Weapon Wars');
  eMods.Add('Wizard Wars');
  eMods.Add('WormsHL');
  eMods.Add('Zombie Panic!');
  eMods.Add('Earth''s Special Forces');
// ... and their pathes ...
  eModsPath.Add('action');
  eModsPath.Add('ag');
  eModsPath.Add('ag3');
  eModsPath.Add('aghl');
  eModsPath.Add('arg');
  eModsPath.Add('asheep');
  eModsPath.Add('bg');
  eModsPath.Add('bot');
  eModsPath.Add('bumpercars');
  eModsPath.Add('buzzybots');
  eModsPath.Add('cs13');
  eModsPath.Add('cstrike');
  eModsPath.Add('csv15');
  eModsPath.Add('czero');
  eModsPath.Add('dcrisis');
  eModsPath.Add('dmc');
  eModsPath.Add('dod');
  eModsPath.Add('dpb');
  eModsPath.Add('dragonmodz');
  eModsPath.Add('esf');
  eModsPath.Add('existence');
  eModsPath.Add('firearms');
  eModsPath.Add('firearms25');
  eModsPath.Add('freeze');
  eModsPath.Add('frontline');
  eModsPath.Add('gangstawars');
  eModsPath.Add('gangwars');
  eModsPath.Add('gearbox');
  eModsPath.Add('globalwarfare');
  eModsPath.Add('goldeneye');
  eModsPath.Add('hlrally');
  eModsPath.Add('holywars');
  eModsPath.Add('hostileintent');
  eModsPath.Add('ios');
  eModsPath.Add('judgedm');
  eModsPath.Add('kanonball');
  eModsPath.Add('monkeystrike');
  eModsPath.Add('MorbidPR');
  eModsPath.Add('ns');
  eModsPath.Add('nsp');
  eModsPath.Add('oel');
  eModsPath.Add('og');
  eModsPath.Add('ol');
  eModsPath.Add('ops1942');
  eModsPath.Add('osjb');
  eModsPath.Add('outbreak');
  eModsPath.Add('oz');
  eModsPath.Add('paintball');
  eModsPath.Add('penemy');
  eModsPath.Add('phineas');
  eModsPath.Add('ponreturn');
  eModsPath.Add('pvk');
  eModsPath.Add('rc2');
  eModsPath.Add('retrocs');
  eModsPath.Add('rewolf');
  eModsPath.Add('ricochet');
  eModsPath.Add('rockcrowbar');
  eModsPath.Add('rspecies');
  eModsPath.Add('scihunt');
  eModsPath.Add('Ship');
  eModsPath.Add('si');
  eModsPath.Add('snow');
  eModsPath.Add('stargatetc');
  eModsPath.Add('svencoop');
  eModsPath.Add('swarm');
  eModsPath.Add('tfc');
  eModsPath.Add('thewastes');
  eModsPath.Add('timeless');
  eModsPath.Add('tod');
  eModsPath.Add('trainhunters');
  eModsPath.Add('trevenge');
  eModsPath.Add('TS');
  eModsPath.Add('TS');
  eModsPath.Add('tt');
  eModsPath.Add('underworld');
  eModsPath.Add('valve');
  eModsPath.Add('vs');
  eModsPath.Add('wantedhl');
  eModsPath.Add('wasteland');
  eModsPath.Add('weapon_wars');
  eModsPath.Add('wizwars');
  eModsPath.Add('wormshl');
  eModsPath.Add('zp');
  eModsPath.Add('esforce');

finalization

// Free them...
eMods.Free;
eModsPath.Free;

end.
