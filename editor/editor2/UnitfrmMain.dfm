object frmMain: TfrmMain
  Left = 198
  Top = 203
  Width = 870
  Height = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001002020040000000000E80200001600000028000000200000004000
    0000010004000000000000020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF004444
    4444444444444444444444444444444444444444444444444444444444444444
    4444444444444444444444444444444444444444444444444444444444444444
    44446664444444444666644444444444444CCCC644444444CCCC644444444444
    444C88CC6444444CC88C644444444444444C888CC64444CC888C644444444444
    444CC888CC644CC888CC4444444444444444CC888CC6CC888CC4444444444444
    444444C888CCC888CC44444444444444444444CC888C888CC444444444444444
    4444444CC88888CC4444444444444444444444444C888C644444444444444444
    44444444CC888CC644444444444444444444444CC88888CC6444444444444444
    444444CC888C888CC64444444444444444444CC888CCC888CC64444444444444
    4444CC888CC4CC888CC6444444444444444CC888CC444CC888CC644444444444
    444C888CC44444CC888C644444444444444C88CC4444444CC88C644444444444
    444CCCC444444444CCCC44444444444444444444444444444444444444444444
    4444444444444444444444444444444444446664444444446664444444444444
    44444663BB000BB3664444444444444444444466333333366444444444444444
    4444444466666664444444444444444444444444444444444444444444444444
    4444444444444444444444444444444444444444444444444444444444440000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000380000000000000000000000000000000000000000000}
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnConstrainedResize = FormConstrainedResize
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object splFunctions: TSplitter
    Left = 709
    Top = 50
    Height = 446
    Align = alRight
  end
  object atbToolBar: TActionToolBar
    Left = 0
    Top = 24
    Width = 862
    Height = 26
    ActionManager = amMenu
    AllowHiding = False
    Caption = 'Toolbar'
    ColorMap.HighlightColor = 14410210
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = 14410210
    Spacing = 0
  end
  object mmbMenu: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 862
    Height = 24
    UseSystemFont = False
    ActionManager = amMenu
    Caption = 'Menu'
    ColorMap.HighlightColor = 14410210
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = 14410210
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Spacing = 0
  end
  object dtcEditor: TSciDocumentTabControl
    Left = 0
    Top = 50
    Width = 709
    Height = 446
    Editor = sciEditor
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    HotTrack = False
    ShowHint = False
    TabOrder = 2
    OnChange = dtcEditorChange
    OnChanging = dtcEditorChanging
    OnMouseUp = dtcEditorMouseUp
    DefaultExt = '.sma'
    object sciEditor: TScintilla
      Left = 4
      Top = 24
      Width = 701
      Height = 418
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      Align = alClient
      OnModified = sciEditorModified
      OnKeyUp = sciEditorKeyUp
      OnKeyDown = sciEditorKeyDown
      OnKeyPress = sciEditorKeyPress
      OnMouseDown = sciEditorMouseDown
      OnMouseUp = sciEditorMouseUp
      OnMouseMove = sciEditorMouseMove
      EOLStyle = eolCRLF
      Indentation = [KeepIndent]
      IndentWidth = 4
      MarginLeft = 1
      MarginRight = 1
      Caret.ForeColor = clWhite
      Caret.LineBackColor = 16770790
      Caret.LineVisible = True
      Caret.Width = 1
      Caret.Period = 500
      DivOptions.ViewWSpace = sciWsInvisible
      DivOptions.UsePalette = False
      DivOptions.OverType = False
      DivOptions.ViewEOL = False
      DivOptions.EndAtLastLine = True
      DivOptions.ScrollBarH = True
      DivOptions.ScrollBarV = True
      ActiveHotSpot.BackColor = clDefault
      ActiveHotSpot.ForeColor = clDefault
      ActiveHotSpot.Underlined = False
      ActiveHotSpot.SingleLine = False
      Colors.SelFore = clHighlightText
      Colors.SelBack = clHighlight
      Colors.MarkerFore = clWhite
      Colors.MarkerBack = clBtnShadow
      Colors.FoldHi = clBtnFace
      Colors.FoldLo = clWhite
      Colors.BookMarkBack = clGray
      Colors.BookMarkFore = clWhite
      Gutter0.Width = 0
      Gutter0.MarginType = gutLineNumber
      Gutter1.Width = 45
      Gutter1.MarginType = gutLineNumber
      Gutter2.Width = 14
      Gutter2.MarginType = gutSymbol
      WordWrapVisualFlags = []
      WordWrapVisualFlagsLocation = []
      LayoutCache = sciCacheCaret
      HideSelect = False
      EdgeMode = sciEdgeLine
      EdgeColumn = 90
      EdgeColor = clSilver
      WordChars = '_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
      ControlCharSymbol = #0
      BraceHilite = False
      Folding = [foldFold, foldCompact, foldComment, foldAtElse]
      FoldMarkerType = sciMarkArrows
      LanguageManager.LanguageList = <
        item
          Name = 'null'
          Lexer = 'null'
          Styles = <>
          Keywords = <>
          AssignmentOperator = '='
          EndOfStatementOperator = ';'
          CommentBoxStart = '/*'
          CommentBoxEnd = '*/'
          CommentBoxMiddle = '*'
          CommentBlock = '//'
          CommentAtLineStart = True
          CommentStreamStart = '/*'
          CommentStreamEnd = '*/'
          NumStyleBits = 5
        end
        item
          Name = 'SMALL'
          Lexer = 'cppnocase'
          Styles = <
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clGreen
              CharCase = CASE_MIXED
              Name = 'Comment'
              StyleNumber = 1
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clTeal
              CharCase = CASE_MIXED
              Name = 'Directives'
              StyleNumber = 9
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clNavy
              CharCase = CASE_MIXED
              Name = 'Operators'
              StyleNumber = 10
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clBlue
              CharCase = CASE_MIXED
              Name = 'Strings'
              StyleNumber = 6
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clRed
              CharCase = CASE_MIXED
              Name = 'General'
              StyleNumber = 16
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clGreen
              CharCase = CASE_MIXED
              Name = 'Line Comment'
              StyleNumber = 2
            end
            item
              FontSize = 0
              FontStyles = [fsBold, fsUnderline]
              ForeColor = clNavy
              CharCase = CASE_MIXED
              Name = 'Brackets'
              StyleNumber = 34
            end>
          Keywords = <
            item
              KeywordListNumber = 1
              Name = 'Keywords'
              Keywords.Strings = (
                '_jghg_enums'
                'tfc_clearmodel'
                'tfc_getbammo'
                'tfc_setbammo'
                'tfc_setmodel'
                'AddMenuItem'
                'AttachView'
                'CreateEntity'
                'DF_Blocked'
                'DF_ClientCommand'
                'DF_ClientConnect'
                'DF_ClientDisconnect'
                'DF_ClientKill'
                'DF_ClientPutInServer'
                'DF_ClientUserInfoChanged'
                'DF_CreateInstancedBaseline'
                'DF_GameInit'
                'DF_GetGameDescription'
                'DF_GetHullBounds'
                'DF_MetaFunc_CallGameEntity'
                'DF_PM_FindTextureType'
                'DF_ParmsChangeLevel'
                'DF_ParmsNewLevel'
                'DF_PlayerPostThink'
                'DF_PlayerPreThink'
                'DF_RegisterEncoders'
                'DF_ServerDeactivate'
                'DF_SetAbsBox'
                'DF_Spawn'
                'DF_SpectatorConnect'
                'DF_SpectatorDisconnect'
                'DF_SpectatorThink'
                'DF_StartFrame'
                'DF_Sys_Error'
                'DF_Think'
                'DF_Touch'
                'DF_Use'
                'DF_pfnAllowLagCompensation'
                'DispatchKeyValue'
                'DispatchSpawn'
                'EF_AllocString'
                'EF_AngleVectors'
                'EF_AnimationAutomove'
                'EF_BuildSoundMSG'
                'EF_CanSkipPlayer'
                'EF_ChangeLevel'
                'EF_ChangePitch'
                'EF_ChangeYaw'
                'EF_CheckVisibility'
                'EF_CreateEntity'
                'EF_CreateFakeClient'
                'EF_CreateNamedEntity'
                'EF_CrosshairAngle'
                'EF_DecalIndex'
                'EF_DropToFloor'
                'EF_EmitAmbientSound'
                'EF_EmitSound'
                'EF_EntIsOnFloor'
                'EF_EntitiesInPVS'
                'EF_FadeClientVolume'
                'EF_FindClientInPVS'
                'EF_FindEntityByString'
                'EF_FindEntityInSphere'
                'EF_FreeEntPrivateData'
                'EF_GetAimVector'
                'EF_GetAttachment'
                'EF_GetBonePosition'
                'EF_GetClientListening'
                'EF_GetCurrentPlayer'
                'EF_GetEntityIllum'
                'EF_GetPhysicsInfoString'
                'EF_GetPhysicsKeyValue'
                'EF_InfoKeyValue'
                'EF_LightStyle'
                'EF_MakeStatic'
                'EF_MakeVectors'
                'EF_MessageBegin'
                'EF_ModelFrames'
                'EF_ModelIndex'
                'EF_MoveToOrigin'
                'EF_NumberOfEntities'
                'EF_ParticleEffect'
                'EF_PlaybackEvent'
                'EF_PointContents'
                'EF_PrecacheEvent'
                'EF_PrecacheGeneric'
                'EF_PrecacheModel'
                'EF_PrecacheSound'
                'EF_RegUserMsg'
                'EF_RemoveEntity'
                'EF_RunPlayerMove'
                'EF_SetClientKeyValue'
                'EF_SetClientListening'
                'EF_SetClientMaxspeed'
                'EF_SetGroupMask'
                'EF_SetKeyValue'
                'EF_SetModel'
                'EF_SetOrigin'
                'EF_SetPhysicsKeyValue'
                'EF_SetSize'
                'EF_SetView'
                'EF_StaticDecal'
                'EF_SzFromIndex'
                'EF_Time'
                'EF_TraceHull'
                'EF_TraceLine'
                'EF_TraceModel'
                'EF_TraceMonsterHull'
                'EF_TraceSphere'
                'EF_TraceTexture'
                'EF_TraceToss'
                'EF_VecToAngles'
                'EF_VecToYaw'
                'EF_WalkMove'
                'EF_WriteAngle'
                'EF_WriteCoord'
                'ENT_SetModel'
                'ENT_SetOrigin'
                'Entvars_Get_Byte'
                'Entvars_Get_Edict'
                'Entvars_Get_Float'
                'Entvars_Get_Int'
                'Entvars_Get_String'
                'Entvars_Get_Vector'
                'Entvars_Set_Byte'
                'Entvars_Set_Edict'
                'Entvars_Set_Float'
                'Entvars_Set_Int'
                'Entvars_Set_String'
                'Entvars_Set_Vector'
                'FVecIVec'
                'FakeTouch'
                'FindEntity'
                'GetMessageBlock'
                'HLTime'
                'IVecFVec'
                'MessageBlock'
                'PointContents'
                'RadiusDamage'
                'RemoveEntity'
                'ServerFrame'
                'SetSpeak'
                'SetView'
                'TraceLn'
                'TraceNormal'
                'VecDist'
                'VecLength'
                'VecToAngles'
                'VelocityByAim'
                'ViewContents'
                'access'
                'add'
                'angle_vector'
                'attach_view'
                'bomb_defused'
                'bomb_defusing'
                'bomb_explode'
                'bomb_planted'
                'bomb_planting'
                'build_path'
                'call_think'
                'callfunc_begin'
                'callfunc_end'
                'callfunc_push_float'
                'callfunc_push_floatrf'
                'callfunc_push_int'
                'callfunc_push_intrf'
                'callfunc_push_str'
                'change_task'
                'clamp'
                'client_PostThink'
                'client_PreThink'
                'client_authorized'
                'client_built'
                'client_changeclass'
                'client_changeteam'
                'client_cmd'
                'client_command'
                'client_connect'
                'client_damage'
                'client_damage'
                'client_damage'
                'client_damage'
                'client_death'
                'client_death'
                'client_death'
                'client_death'
                'client_disconnect'
                'client_impulse'
                'client_infochanged'
                'client_kill'
                'client_print'
                'client_putinserver'
                'client_score'
                'client_spawn'
                'cmd_access'
                'cmd_target'
                'colored_menus'
                'console_cmd'
                'console_print'
                'const'
                'AMXX_VERSION_STR[]="1'
                'contain'
                'containi'
                'copy'
                'copy_keyvalue'
                'copyc'
                'create_entity'
                'cs_get_hostage_foll'
                'cs_get_hostage_id'
                'cs_get_no_knives'
                'cs_get_user_bpammo'
                'cs_get_user_buyzone'
                'cs_get_user_deaths'
                'cs_get_user_defuse'
                'cs_get_user_driving'
                'cs_get_user_hasprim'
                'cs_get_user_model'
                'cs_get_user_money'
                'cs_get_user_nvg'
                'cs_get_user_plant'
                'cs_get_user_stationary'
                'cs_get_user_team'
                'cs_get_user_tked'
                'cs_get_user_vip'
                'cs_get_weapon_ammo'
                'cs_get_weapon_burst'
                'cs_get_weapon_id'
                'cs_get_weapon_silen'
                'cs_reset_user_model'
                'cs_set_hostage_foll'
                'cs_set_no_knives'
                'cs_set_user_bpammo'
                'cs_set_user_deaths'
                'cs_set_user_defuse'
                'cs_set_user_model'
                'cs_set_user_money'
                'cs_set_user_nvg'
                'cs_set_user_plant'
                'cs_set_user_team'
                'cs_set_user_tked'
                'cs_set_user_vip'
                'cs_set_weapon_ammo'
                'cs_set_weapon_burst'
                'cs_set_weapon_silen'
                'cstrike_running'
                'current_num_ents'
                'custom_weapon_add'
                'custom_weapon_add'
                'custom_weapon_add'
                'custom_weapon_add'
                'custom_weapon_dmg'
                'custom_weapon_dmg'
                'custom_weapon_dmg'
                'custom_weapon_dmg'
                'custom_weapon_shot'
                'custom_weapon_shot'
                'custom_weapon_shot'
                'custom_weapon_shot'
                'cvar_exists'
                'date'
                'dbi_close'
                'dbi_connect'
                'dbi_error'
                'dbi_field'
                'dbi_free_result'
                'dbi_nextrow'
                'dbi_num_rows'
                'dbi_query'
                'dbi_result'
                'dbi_type'
                'delete_file'
                'dllfunc'
                'dod_get_map_info'
                'dod_get_next_class'
                'dod_get_pl_deaths'
                'dod_get_pl_teamname'
                'dod_get_pronestate'
                'dod_get_team_score'
                'dod_get_user_ammo'
                'dod_get_user_class'
                'dod_get_user_kills'
                'dod_get_user_score'
                'dod_get_user_weapon'
                'dod_is_deployed'
                'dod_is_randomclass'
                'dod_make_deathmsg'
                'dod_set_fuse'
                'dod_set_pl_deaths'
                'dod_set_pl_teamname'
                'dod_set_stamina'
                'dod_set_user_ammo'
                'dod_set_user_class'
                'dod_set_user_kills'
                'dod_set_user_score'
                'dod_set_user_team'
                'dod_user_kill'
                'dod_wpnlog_to_id'
                'dod_wpnlog_to_name'
                'drop_to_floor'
                'emit_sound'
                'engclient_cmd'
                'engclient_print'
                'engfunc'
                'entity_count'
                'entity_get_byte'
                'entity_get_edict'
                'entity_get_float'
                'entity_get_int'
                'entity_get_string'
                'entity_get_vector'
                'entity_range'
                'entity_set_byte'
                'entity_set_edict'
                'entity_set_float'
                'entity_set_int'
                'entity_set_model'
                'entity_set_origin'
                'entity_set_size'
                'entity_set_string'
                'entity_set_vector'
                'equal'
                'equali'
                'fake_touch'
                'fakedamage'
                'fclose'
                'feof'
                'fflush'
                'fgetc'
                'fgetf'
                'fgeti'
                'fgetl'
                'fgets'
                'file_exists'
                'file_size'
                'filesize'
                'find_ent'
                'find_ent_by_class'
                'find_ent_by_model'
                'find_ent_by_owner'
                'find_ent_by_target'
                'find_ent_by_tname'
                'find_ent_in_sphere'
                'find_ent_sphere'
                'find_entity'
                'find_player'
                'find_plugin_bydesc'
                'find_plugin_byfile'
                'find_sphere_class'
                'float'
                'floatabs'
                'floatacos'
                'floatadd'
                'floatasin'
                'floatatan'
                'floatatan2'
                'floatcmp'
                'floatcos'
                'floatdiv'
                'floatfract'
                'floatlog'
                'floatmul'
                'floatpower'
                'floatround'
                'floatsin'
                'floatsqroot'
                'floatstr'
                'floatsub'
                'floattan'
                'fopen'
                'force_unmodified'
                'force_use'
                'format'
                'format_args'
                'format_time'
                'forward_return'
                'fputc'
                'fputf'
                'fputi'
                'fputl'
                'fputs'
                'fread'
                'fscanf'
                'fseek'
                'ftell'
                'funcidx'
                'fwrite'
                'geoip_code2'
                'geoip_code3'
                'geoip_country'
                'get_basedir'
                'get_brush_entity_origin'
                'get_build'
                'get_class'
                'get_clcmd'
                'get_clcmdsnum'
                'get_client_listen'
                'get_concmd'
                'get_concmdsnum'
                'get_configsdir'
                'get_customdir'
                'get_cvar_flags'
                'get_cvar_float'
                'get_cvar_num'
                'get_cvar_string'
                'get_datadir'
                'get_decal_index'
                'get_distance'
                'get_entity_distance'
                'get_entity_flags'
                'get_entity_origin'
                'get_entity_velocity'
                'get_entity_visibility'
                'get_filename'
                'get_flags'
                'get_gametime'
                'get_global_edict'
                'get_global_float'
                'get_global_int'
                'get_global_string'
                'get_global_vector'
                'get_grenade'
                'get_grenade_id'
                'get_grenade_index'
                'get_hostage_id'
                'get_info_keybuffer'
                'get_keyvalue'
                'get_lang'
                'get_langsnum'
                'get_localinfo'
                'get_logfile'
                'get_mapname'
                'get_mask'
                'get_max_entities'
                'get_maxplayers'
                'get_maxspeed'
                'get_modname'
                'get_module'
                'get_modulesnum'
                'get_msg_arg_float'
                'get_msg_arg_int'
                'get_msg_arg_string'
                'get_msg_args'
                'get_msg_argtype'
                'get_msg_block'
                'get_msg_origin'
                'get_owner'
                'get_pdata'
                'get_pdata_char'
                'get_pdata_float'
                'get_pdata_int'
                'get_pdata_short'
                'get_players'
                'get_playersnum'
                'get_plugin'
                'get_pluginsnum'
                'get_private_f'
                'get_private_i'
                'get_range'
                'get_res'
                'get_spawn'
                'get_speak'
                'get_special'
                'get_speed'
                'get_speedchange'
                'get_srvcmd'
                'get_srvcmdsnum'
                'get_stats'
                'get_stats'
                'get_stats'
                'get_stats'
                'get_stats2'
                'get_statsnum'
                'get_statsnum'
                'get_statsnum'
                'get_statsnum'
                'get_string'
                'get_systime'
                'get_time'
                'get_timeleft'
                'get_tr'
                'get_user_aiming'
                'get_user_ammo'
                'get_user_armor'
                'get_user_astats'
                'get_user_astats'
                'get_user_astats'
                'get_user_astats'
                'get_user_attacker'
                'get_user_authid'
                'get_user_button'
                'get_user_deaths'
                'get_user_flags'
                'get_user_frags'
                'get_user_godmode'
                'get_user_gravity'
                'get_user_health'
                'get_user_hitzones'
                'get_user_index'
                'get_user_info'
                'get_user_ip'
                'get_user_lstats'
                'get_user_maxspeed'
                'get_user_menu'
                'get_user_money'
                'get_user_msgid'
                'get_user_msgname'
                'get_user_name'
                'get_user_noclip'
                'get_user_oldbutton'
                'get_user_origin'
                'get_user_ping'
                'get_user_rstats'
                'get_user_rstats'
                'get_user_rstats'
                'get_user_rstats'
                'get_user_stats'
                'get_user_stats'
                'get_user_stats'
                'get_user_stats'
                'get_user_stats2'
                'get_user_team'
                'get_user_time'
                'get_user_userid'
                'get_user_velocity'
                'get_user_vstats'
                'get_user_vstats'
                'get_user_vstats'
                'get_user_vstats'
                'get_user_weapon'
                'get_user_weapons'
                'get_user_wlstats'
                'get_user_wlstats'
                'get_user_wrstats'
                'get_user_wrstats'
                'get_user_wrstats'
                'get_user_wrstats'
                'get_user_wstats'
                'get_user_wstats'
                'get_user_wstats'
                'get_user_wstats'
                'get_usercmd'
                'get_vaultdata'
                'get_weaponname'
                'get_xvar_float'
                'get_xvar_id'
                'get_xvar_num'
                'getarg'
                'getkey_float'
                'getkey_int'
                'getkey_string'
                'give_item'
                'globals_get_edict'
                'globals_get_float'
                'globals_get_int'
                'globals_get_string'
                'globals_get_vector'
                'gpglobals_v'
                'gpgobals_time'
                'grenade_throw'
                'grenade_throw'
                'halflife_time'
                'has_weapon'
                'heapspace'
                'in_list_float'
                'in_list_int'
                'in_list_string'
                'inconsistent_file'
                'is_combat'
                'is_dedicated_server'
                'is_ent_valid'
                'is_entity'
                'is_jit_enabled'
                'is_linux_server'
                'is_map_valid'
                'is_module_loaded'
                'is_plugin_loaded'
                'is_running'
                'is_user_admin'
                'is_user_alive'
                'is_user_bot'
                'is_user_connected'
                'is_user_connecting'
                'is_user_hltv'
                'is_valid_ent'
                'isalnum'
                'isalpha'
                'isdigit'
                'isspace'
                'jghg2_set_size'
                'jghg2_think'
                'jghg_find_ent_owner'
                'keytable_clear'
                'keytable_count'
                'keytable_delete'
                'keytable_getkey'
                'keytable_getval'
                'keytable_next'
                'keytable_reset'
                'lang_exists'
                'list_clear'
                'list_clear_float'
                'list_clear_int'
                'list_clear_string'
                'list_delete'
                'list_delete_float'
                'list_delete_int'
                'list_delete_string'
                'list_get'
                'list_get_float'
                'list_get_int'
                'list_get_string'
                'list_getf'
                'list_next'
                'list_next_float'
                'list_next_int'
                'list_next_string'
                'list_pop'
                'list_pop_float'
                'list_pop_int'
                'list_pop_string'
                'list_push_float'
                'list_push_int'
                'list_push_string'
                'list_reset'
                'list_reset_float'
                'list_reset_int'
                'list_reset_string'
                'list_size'
                'list_size_float'
                'list_size_int'
                'list_size_string'
                'list_store_float'
                'list_store_int'
                'list_store_string'
                'log_amx'
                'log_message'
                'log_to_file'
                'make_deathmsg'
                'make_string'
                'max'
                'md5'
                'md5_file'
                'message_begin'
                'message_end'
                'min'
                'msg_args'
                'msg_data'
                'msg_data_type'
                'msg_dest'
                'msg_loc'
                'msg_name'
                'msg_set_f'
                'msg_set_i'
                'msg_set_s'
                'msg_strdata'
                'msg_type'
                'mysql_close'
                'mysql_connect'
                'mysql_error'
                'mysql_getfield'
                'mysql_nextrow'
                'mysql_query'
                'new_float_list'
                'new_int_list'
                'new_keytable'
                'new_list'
                'new_string_list'
                'ns2amx_getammo'
                'ns2amx_getenergy'
                'ns2amx_gethives'
                'ns2amx_getjpfuel'
                'ns2amx_giveitem'
                'ns2amx_inrange'
                'ns2amx_isdigesting'
                'ns2amx_moveto'
                'ns2amx_nspopup'
                'ns2amx_setammo'
                'ns2amx_setenergy'
                'ns2amx_setjpfuel'
                'ns2amx_setres'
                'ns2amx_version'
                'ns_get_build'
                'ns_get_class'
                'ns_get_deaths'
                'ns_get_energy'
                'ns_get_exp'
                'ns_get_hive_trait'
                'ns_get_jpfuel'
                'ns_get_mask'
                'ns_get_maxspeed'
                'ns_get_points'
                'ns_get_res'
                'ns_get_score'
                'ns_get_spawn'
                'ns_get_speedchange'
                'ns_get_struct_owner'
                'ns_get_weap_clip'
                'ns_get_weap_dmg'
                'ns_get_weap_range'
                'ns_get_weap_reserve'
                'ns_give_item'
                'ns_has_weapon'
                'ns_is_combat'
                'ns_popup'
                'ns_set_deaths'
                'ns_set_energy'
                'ns_set_exp'
                'ns_set_fov'
                'ns_set_hive_trait'
                'ns_set_jpfuel'
                'ns_set_mask'
                'ns_set_player_body'
                'ns_set_player_model'
                'ns_set_player_skin'
                'ns_set_points'
                'ns_set_res'
                'ns_set_score'
                'ns_set_speedchange'
                'ns_set_struct_owner'
                'ns_set_weap_clip'
                'ns_set_weap_dmg'
                'ns_set_weap_range'
                'ns_set_weap_reserve'
                'num_to_str'
                'num_to_word'
                'numargs'
                'number_of_entities'
                'numtostr'
                'parse'
                'parse_loguser'
                'parse_time'
                'pause'
                'pev'
                'pev_f'
                'pev_i'
                'pfn_keyvalue'
                'pfn_playbackevent'
                'pfn_spawn'
                'pfn_think'
                'pfn_touch'
                'playback_event'
                'plugin_cfg'
                'plugin_end'
                'plugin_flags'
                'plugin_init'
                'plugin_log'
                'plugin_modules'
                'plugin_pause'
                'plugin_precache'
                'plugin_unpause'
                'point_contents'
                'power'
                'precache_event'
                'precache_generic'
                'precache_model'
                'precache_sound'
                'radius_damage'
                'random'
                'random_float'
                'random_num'
                'read_argc'
                'read_args'
                'read_argv'
                'read_data'
                'read_datanum'
                'read_dir'
                'read_file'
                'read_flags'
                'read_logargc'
                'read_logargv'
                'read_logdata'
                'regex_free'
                'regex_match'
                'regex_substr'
                'register_changelvl'
                'register_clcmd'
                'register_clientkill'
                'register_concmd'
                'register_cvar'
                'register_dictionary'
                'register_event'
                'register_forward'
                'register_impulse'
                'register_logevent'
                'register_menu'
                'register_menucmd'
                'register_menuid'
                'register_message'
                'register_msgblock'
                'register_msgedit'
                'register_playback'
                'register_plugin'
                'register_srvcmd'
                'register_statsfwd'
                'register_statsfwd'
                'register_statsfwd'
                'register_think'
                'register_touch'
                'remove_cvar_flags'
                'remove_entity'
                'remove_entity_name'
                'remove_quotes'
                'remove_task'
                'remove_user_flags'
                'remove_vaultdata'
                'replace'
                'require_module'
                'reset_user_wstats'
                'reset_user_wstats'
                'reset_user_wstats'
                'reset_user_wstats'
                'rewind'
                'server_changelevel'
                'server_cmd'
                'server_exec'
                'server_frame'
                'server_print'
                'set_client_listen'
                'set_cvar_flags'
                'set_cvar_float'
                'set_cvar_num'
                'set_cvar_string'
                'set_entity_flags'
                'set_entity_origin'
                'set_entity_velocity'
                'set_entity_visibility'
                'set_hudmessage'
                'set_kvhandled'
                'set_lights'
                'set_localinfo'
                'set_mask'
                'set_msg_arg_float'
                'set_msg_arg_int'
                'set_msg_arg_string'
                'set_msg_block'
                'set_pdata'
                'set_pdata_char'
                'set_pdata_float'
                'set_pdata_int'
                'set_pdata_short'
                'set_pev'
                'set_pev_f'
                'set_pev_i'
                'set_player_body'
                'set_player_model'
                'set_player_skin'
                'set_private_f'
                'set_private_i'
                'set_rendering'
                'set_size'
                'set_speak'
                'set_speedchange'
                'set_task'
                'set_tr'
                'set_user_armor'
                'set_user_deaths'
                'set_user_flags'
                'set_user_footsteps'
                'set_user_frags'
                'set_user_godmode'
                'set_user_gravity'
                'set_user_health'
                'set_user_hitzones'
                'set_user_info'
                'set_user_maxspeed'
                'set_user_money'
                'set_user_noclip'
                'set_user_origin'
                'set_user_rendering'
                'set_user_velocity'
                'set_usercmd'
                'set_vaultdata'
                'set_view'
                'set_xvar_float'
                'set_xvar_num'
                'setarg'
                'setc'
                'show_activity'
                'show_hudmessage'
                'show_menu'
                'show_motd'
                'socket_change'
                'socket_close'
                'socket_open'
                'socket_recv'
                'socket_send'
                'spawn'
                'sqroot'
                'store_float'
                'store_int'
                'store_string'
                'str_to_num'
                'strbreak'
                'string'
                'strip_user_weapons'
                'strlen'
                'strpack'
                'strtok'
                'strtolower'
                'strtonum'
                'strtoupper'
                'strunpack'
                'supercede'
                'swapchars'
                'take_damage'
                'task_exists'
                'tfc_getweaponbammo'
                'tfc_isgrenade'
                'tfc_setpddata'
                'tfc_setweaponbammo'
                'tfc_userkill'
                'tickcount'
                'time'
                'tolower'
                'toupper'
                'trace_hull'
                'trace_line'
                'trace_normal'
                'traceresult'
                'trim'
                'ts_createpwup'
                'ts_getkillingstreak'
                'ts_getusercash'
                'ts_getuseritems'
                'ts_getuserkillflags'
                'ts_getuserlastfrag'
                'ts_getuserpwup'
                'ts_getuserspace'
                'ts_getuserwpn'
                'ts_givepwup'
                'ts_giveweapon'
                'ts_setpddata'
                'ts_wpnlogtoid'
                'ts_wpnlogtoname'
                'ucfirst'
                'unlink'
                'unpause'
                'use'
                'user_has_weapon'
                'user_kill'
                'user_silentkill'
                'user_slap'
                'user_spawn'
                'vaultdata_exists'
                'vector_distance'
                'vector_length'
                'vector_to_angle'
                'velocity_by_aim'
                'vexd_pfntouch'
                'write_angle'
                'write_byte'
                'write_char'
                'write_coord'
                'write_entity'
                'write_file'
                'write_long'
                'write_short'
                'write_string'
                'xmod_get_maxweapons'
                'xmod_get_maxweapons'
                'xmod_get_maxweapons'
                'xmod_get_maxweapons'
                'xmod_get_stats_size'
                'xmod_get_stats_size'
                'xmod_get_stats_size'
                'xmod_get_stats_size'
                'xmod_get_wpnlogname'
                'xmod_get_wpnlogname'
                'xmod_get_wpnlogname'
                'xmod_get_wpnlogname'
                'xmod_get_wpnname'
                'xmod_get_wpnname'
                'xmod_get_wpnname'
                'xmod_get_wpnname'
                'xmod_is_custom_wpn'
                'xmod_is_custom_wpn'
                'xmod_is_custom_wpn'
                'xmod_is_melee_wpn'
                'xmod_is_melee_wpn'
                'xmod_is_melee_wpn'
                'xmod_is_melee_wpn'
                'xvar_exists'
                'ADMIN_ADMIN'
                'ADMIN_ALL'
                'ADMIN_BAN'
                'ADMIN_CFG'
                'ADMIN_CHAT'
                'ADMIN_CVAR'
                'ADMIN_IMMUNITY'
                'ADMIN_KICK'
                'ADMIN_LEVEL_A'
                'ADMIN_LEVEL_B'
                'ADMIN_LEVEL_C'
                'ADMIN_LEVEL_D'
                'ADMIN_LEVEL_E'
                'ADMIN_LEVEL_F'
                'ADMIN_LEVEL_G'
                'ADMIN_LEVEL_H'
                'ADMIN_MAP'
                'ADMIN_MENU'
                'ADMIN_PASSWORD'
                'ADMIN_RCON'
                'ADMIN_RESERVATION'
                'ADMIN_SLAY'
                'ADMIN_USER'
                'ADMIN_VOTE'
                'ALLIES'
                'AMXX_VERSION'
                'AMX_FLAG_BIGENDIAN'
                'AMX_FLAG_BROWSE'
                'AMX_FLAG_COMPACT'
                'AMX_FLAG_DEBUG'
                'AMX_FLAG_LINEOPS'
                'AMX_FLAG_NOCHECKS'
                'AMX_FLAG_RELOC'
                'ATTN_IDLE'
                'ATTN_NONE'
                'ATTN_STATIC'
                'AXIS'
                'BLOCK_NOT'
                'BLOCK_ONCE'
                'BLOCK_SET'
                'CAMERA_3RDPERSON'
                'CAMERA_NONE'
                'CAMERA_TOPDOWN'
                'CAMERA_UPLEFT'
                'CHAN_AUTO'
                'CHAN_ITEM'
                'CHAN_NETWORKVOICE_BASE'
                'CHAN_NETWORKVOICE_END'
                'CHAN_STATIC'
                'CHAN_STREAM'
                'CHAN_WEAPON'
                'CONTENTS_TRANSLUCENT'
                'CSW_AK47'
                'CSW_AUG'
                'CSW_AWP'
                'CSW_C4'
                'CSW_DEAGLE'
                'CSW_ELITE'
                'CSW_FAMAS'
                'CSW_FIVESEVEN'
                'CSW_FLASHBANG'
                'CSW_G3SG1'
                'CSW_GALI'
                'CSW_GALIL'
                'CSW_GLOCK18'
                'CSW_HEGRENADE'
                'CSW_KNIFE'
                'CSW_M249'
                'CSW_M3'
                'CSW_M4A1'
                'CSW_MAC10'
                'CSW_MP5NAVY'
                'CSW_P228'
                'CSW_P90'
                'CSW_SCOUT'
                'CSW_SG550'
                'CSW_SG552'
                'CSW_SMOKEGRENADE'
                'CSW_TMP'
                'CSW_UMP45'
                'CSW_USP'
                'CSW_XM1014'
                'DMG_ACID'
                'DMG_ALWAYSGIB'
                'DMG_BLAST'
                'DMG_BULLET'
                'DMG_BURN'
                'DMG_CLUB'
                'DMG_CRUSH'
                'DMG_DROWN'
                'DMG_DROWNRECOVER'
                'DMG_ENERGYBEAM'
                'DMG_FALL'
                'DMG_FREEZE'
                'DMG_GENERIC'
                'DMG_MORTAR'
                'DMG_NERVEGAS'
                'DMG_NEVERGIB'
                'DMG_PARALYZE'
                'DMG_POISON'
                'DMG_RADIATION'
                'DMG_SHOCK'
                'DMG_SLASH'
                'DMG_SLOWBURN'
                'DMG_SLOWFREEZE'
                'DMG_SONIC'
                'DMG_TIMEBASED'
                'DODMAX_WEAPONS'
                'EF_INVLIGHT'
                'EF_LIGHT'
                'EF_NODRAW'
                'EF_NOINTERP'
                'FCVAR_CLIENTDLL'
                'FCVAR_EXTDLL'
                'FCVAR_PRINTABLEONLY'
                'FCVAR_PROTECTED'
                'FCVAR_SPONLY'
                'FCVAR_UNLOGGED'
                'FLAG_AUTHID'
                'FLAG_IP'
                'FLAG_KICK'
                'FLAG_NOPASS'
                'FLAG_TAG'
                'FL_ALWAYSTHINK'
                'FL_BASEVELOCITY'
                'FL_CUSTOMENTITY'
                'FL_DORMANT'
                'FL_DUCKING'
                'FL_FAKECLIENT'
                'FL_FLOAT'
                'FL_FROZEN'
                'FL_GRAPHED'
                'FL_IMMUNE_LAVA'
                'FL_IMMUNE_WATER'
                'FL_KILLME'
                'FL_MONSTERCLIP'
                'FL_ONTRAIN'
                'FL_PROXY'
                'FL_SPECTATOR'
                'FL_WORLDBRUSH'
                'FMRES_HANDLED'
                'FMRES_IGNORED'
                'FMRES_OVERRIDE'
                'FMRES_SUPERCEDE'
                'FMV_CELL'
                'FMV_FLOAT'
                'FT_NEW'
                'FT_OLD'
                'FUSE_RESET'
                'FUSE_SET'
                'HIT_CHEST'
                'HIT_GENERIC'
                'HIT_HEAD'
                'HIT_LEFTARM'
                'HIT_LEFTLEG'
                'HIT_RIGHTARM'
                'HIT_RIGHTLEG'
                'HIT_STOMACH'
                'HIW_AK47'
                'HIW_AKS74U'
                'HIW_BERETTA'
                'HIW_FLASHBANG'
                'HIW_GLOCK'
                'HIW_M11'
                'HIW_M11SD'
                'HIW_M16A2'
                'HIW_M4A1'
                'HIW_MP5A4'
                'HIW_MP5SD5'
                'HIW_NATOGREN'
                'HIW_PSG1'
                'HIW_REMINGTON'
                'HIW_SPAS12'
                'HIW_TANGOGREN'
                'HIW_ZASTAVA'
                'HULL_HEAD'
                'HULL_HUMAN'
                'HULL_LARGE'
                'HULL_POINT'
                'IN_ALT1'
                'IN_ATTACK'
                'IN_ATTACK2'
                'IN_BACK'
                'IN_CANCEL'
                'IN_DUCK'
                'IN_FORWARD'
                'IN_JUMP'
                'IN_LEFT'
                'IN_MOVELEFT'
                'IN_MOVERIGHT'
                'IN_RELOAD'
                'IN_RIGHT'
                'IN_RUN'
                'IN_SCORE'
                'IN_USE'
                'LANG_PLAYER'
                'LANG_SERVER'
                'MENU_KEY_0'
                'MENU_KEY_1'
                'MENU_KEY_2'
                'MENU_KEY_3'
                'MENU_KEY_4'
                'MENU_KEY_5'
                'MENU_KEY_6'
                'MENU_KEY_7'
                'MENU_KEY_8'
                'MENU_KEY_9'
                'MOVETYPE_ANGLECLIP'
                'MOVETYPE_ANGLENOCLIP'
                'MOVETYPE_BOUNCEMISSILE'
                'MOVETYPE_FOLLOW'
                'MSG_ONE_UNRELIABLE'
                'MSG_PAS'
                'MSG_PAS_R'
                'MSG_PVS'
                'MSG_PVS_R'
                'NS_CONST_INC'
                'NS_INC'
                'PITCH_HIGH'
                'PITCH_LOW'
                'PLUGIN_CONTINUE'
                'PLUGIN_HANDLED'
                'PLUGIN_HANDLED_MAIN'
                'SEEK_CUR'
                'SEEK_END'
                'SEEK_SET'
                'SOCKET_TCP'
                'SOCKET_UDP'
                'SPEAK_ALL'
                'SPEAK_LISTENALL'
                'SPEAK_MUTED'
                'SPEAK_NORMAL'
                'STAMINA_RESET'
                'STAMINA_SET'
                'SVC_ADDANGLE'
                'SVC_CDTRACK'
                'SVC_INTERMISSION'
                'SVC_NEWUSERMSG'
                'SVC_ROOMTYPE'
                'SVC_TEMPENTITY'
                'SVC_WEAPONANIM'
                'TFCMAX_WEAPONS'
                'TSA_FLASHLIGHT'
                'TSA_LASERSIGHT'
                'TSA_SCOPE'
                'TSA_SILENCER'
                'TSITEM_KUNGFU'
                'TSITEM_SUPERJUMP'
                'TSKF_DOUBLEKILL'
                'TSKF_ISSPEC'
                'TSKF_KILLEDSPEC'
                'TSKF_SLIDINGKILL'
                'TSKF_STUNTKILL'
                'TSMAX_WEAPONS'
                'TSPWUP_ARMOR'
                'TSPWUP_DFIRERATE'
                'TSPWUP_GRENADE'
                'TSPWUP_HEALTH'
                'TSPWUP_INFAMMO'
                'TSPWUP_KUNGFU'
                'TSPWUP_RANDOM'
                'TSPWUP_SLOWMO'
                'TSPWUP_SLOWPAUSE'
                'TSPWUP_SUPERJUMP'
                'VOL_NORM'
                'XS_AMX'
                'XS_AMXX'
                'XS__LIBRELEASE'
                'assert'
                'break'
                'case'
                'char'
                'const'
                'continue'
                'default'
                'defined'
                'do'
                'else'
                'enum'
                'exit'
                'for'
                'forward'
                'goto'
                'if'
                'native'
                'new'
                'operator'
                'public'
                'return'
                'sizeof'
                'sleep'
                'static'
                'stock'
                'switch'
                'while')
            end>
          AssignmentOperator = '='
          EndOfStatementOperator = ';'
          CommentBoxStart = '/*'
          CommentBoxEnd = '*/'
          CommentBoxMiddle = '*'
          CommentBlock = '//'
          CommentAtLineStart = True
          CommentStreamStart = '/*'
          CommentStreamEnd = '*/'
          NumStyleBits = 5
        end
        item
          Name = 'container'
          Lexer = 'container'
          Styles = <>
          Keywords = <>
          AssignmentOperator = '='
          EndOfStatementOperator = ';'
          CommentBoxStart = '/*'
          CommentBoxEnd = '*/'
          CommentBoxMiddle = '*'
          CommentBlock = '//'
          CommentAtLineStart = True
          CommentStreamStart = '/*'
          CommentStreamEnd = '*/'
          NumStyleBits = 5
        end>
      LanguageManager.SelectedLanguage = 'null'
      FoldDrawFlags = [sciBelowIfNotExpanded]
      KeyCommands = <
        item
          Command = 2300
          ShortCut = 40
        end
        item
          Command = 2301
          ShortCut = 8232
        end
        item
          Command = 2342
          ShortCut = 16424
        end
        item
          Command = 2426
          ShortCut = 41000
        end
        item
          Command = 2302
          ShortCut = 38
        end
        item
          Command = 2303
          ShortCut = 8230
        end
        item
          Command = 2343
          ShortCut = 16422
        end
        item
          Command = 2427
          ShortCut = 40998
        end
        item
          Command = 2415
          ShortCut = 49190
        end
        item
          Command = 2416
          ShortCut = 57382
        end
        item
          Command = 2413
          ShortCut = 49192
        end
        item
          Command = 2414
          ShortCut = 57384
        end
        item
          Command = 2304
          ShortCut = 37
        end
        item
          Command = 2305
          ShortCut = 8229
        end
        item
          Command = 2308
          ShortCut = 16421
        end
        item
          Command = 2309
          ShortCut = 24613
        end
        item
          Command = 2428
          ShortCut = 40997
        end
        item
          Command = 2306
          ShortCut = 39
        end
        item
          Command = 2307
          ShortCut = 8231
        end
        item
          Command = 2310
          ShortCut = 16423
        end
        item
          Command = 2311
          ShortCut = 24615
        end
        item
          Command = 2429
          ShortCut = 40999
        end
        item
          Command = 2390
          ShortCut = 49189
        end
        item
          Command = 2391
          ShortCut = 57381
        end
        item
          Command = 2392
          ShortCut = 49191
        end
        item
          Command = 2393
          ShortCut = 57383
        end
        item
          Command = 2331
          ShortCut = 36
        end
        item
          Command = 2332
          ShortCut = 8228
        end
        item
          Command = 2316
          ShortCut = 16420
        end
        item
          Command = 2317
          ShortCut = 24612
        end
        item
          Command = 2345
          ShortCut = 32804
        end
        item
          Command = 2431
          ShortCut = 40996
        end
        item
          Command = 2314
          ShortCut = 35
        end
        item
          Command = 2315
          ShortCut = 8227
        end
        item
          Command = 2318
          ShortCut = 16419
        end
        item
          Command = 2319
          ShortCut = 24611
        end
        item
          Command = 2347
          ShortCut = 32803
        end
        item
          Command = 2432
          ShortCut = 40995
        end
        item
          Command = 2320
          ShortCut = 33
        end
        item
          Command = 2321
          ShortCut = 8225
        end
        item
          Command = 2433
          ShortCut = 40993
        end
        item
          Command = 2322
          ShortCut = 34
        end
        item
          Command = 2323
          ShortCut = 8226
        end
        item
          Command = 2434
          ShortCut = 40994
        end
        item
          Command = 2180
          ShortCut = 46
        end
        item
          Command = 2177
          ShortCut = 8238
        end
        item
          Command = 2336
          ShortCut = 16430
        end
        item
          Command = 2396
          ShortCut = 24622
        end
        item
          Command = 2324
          ShortCut = 45
        end
        item
          Command = 2179
          ShortCut = 8237
        end
        item
          Command = 2178
          ShortCut = 16429
        end
        item
          Command = 2325
          ShortCut = 27
        end
        item
          Command = 2326
          ShortCut = 8
        end
        item
          Command = 2326
          ShortCut = 8200
        end
        item
          Command = 2335
          ShortCut = 16392
        end
        item
          Command = 2176
          ShortCut = 32776
        end
        item
          Command = 2395
          ShortCut = 24584
        end
        item
          Command = 2176
          ShortCut = 16474
        end
        item
          Command = 2011
          ShortCut = 16473
        end
        item
          Command = 2177
          ShortCut = 16472
        end
        item
          Command = 2178
          ShortCut = 16451
        end
        item
          Command = 2179
          ShortCut = 16470
        end
        item
          Command = 2013
          ShortCut = 16449
        end
        item
          Command = 2327
          ShortCut = 9
        end
        item
          Command = 2328
          ShortCut = 8201
        end
        item
          Command = 2329
          ShortCut = 13
        end
        item
          Command = 2329
          ShortCut = 8205
        end
        item
          Command = 2333
          ShortCut = 16491
        end
        item
          Command = 2334
          ShortCut = 16493
        end
        item
          Command = 2373
          ShortCut = 16495
        end
        item
          Command = 2337
          ShortCut = 16460
        end
        item
          Command = 2338
          ShortCut = 24652
        end
        item
          Command = 2455
          ShortCut = 24660
        end
        item
          Command = 2339
          ShortCut = 16468
        end
        item
          Command = 2404
          ShortCut = 16452
        end
        item
          Command = 2340
          ShortCut = 16469
        end
        item
          Command = 2341
          ShortCut = 24661
        end>
    end
  end
  object sbInfo: TStatusBar
    Left = 0
    Top = 594
    Width = 862
    Height = 19
    AutoHint = True
    Panels = <
      item
        Text = 'Unitled.sma'
        Width = 700
      end
      item
        Alignment = taCenter
        Width = 50
      end
      item
        Alignment = taCenter
        Text = 'Ln 1 Ch 1'
        Width = 50
      end>
  end
  object pnlFunctions: TPanel
    Left = 712
    Top = 50
    Width = 150
    Height = 446
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 4
    object pnlSpacerTop: TPanel
      Left = 0
      Top = 0
      Width = 150
      Height = 21
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
    end
    object trvFunctions: TTreeView
      Left = 0
      Top = 21
      Width = 143
      Height = 423
      Align = alClient
      Images = ilMenu
      Indent = 19
      ParentShowHint = False
      ShowHint = True
      SortType = stText
      TabOrder = 1
      OnClick = trvFunctionsClick
      OnCollapsed = trvFunctionsCollapsed
      OnDblClick = trvFunctionsDblClick
      OnEdited = trvFunctionsEdited
      OnEditing = trvFunctionsEditing
      OnEnter = lvDebugEnter
      OnExpanded = trvFunctionsExpanded
      OnKeyDown = trvFunctionsKeyDown
      Items.Data = {
        05000000220000001200000012000000FFFFFFFFFFFFFFFF0000000000000000
        09436F6E7374616E7473200000001200000012000000FFFFFFFFFFFFFFFF0000
        00000000000007446566696E6564220000001200000012000000FFFFFFFFFFFF
        FFFF00000000000000000946756E6374696F6E73210000001200000012000000
        FFFFFFFFFFFFFFFF000000000000000008496E636C7564656422000000120000
        0012000000FFFFFFFFFFFFFFFF0000000000000000095661726961626C6573}
    end
    object pnlSpacerLeft: TPanel
      Left = 143
      Top = 21
      Width = 7
      Height = 423
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      OnClick = pnlSpacerLeftClick
    end
    object pnlSpacerBottom: TPanel
      Left = 0
      Top = 444
      Width = 150
      Height = 2
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
    end
  end
  object lvDebug: TListView
    Left = 0
    Top = 496
    Width = 862
    Height = 98
    Align = alBottom
    Columns = <
      item
        Width = 858
      end>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    FlatScrollBars = True
    ReadOnly = True
    RowSelect = True
    ParentFont = False
    ShowColumnHeaders = False
    TabOrder = 5
    ViewStyle = vsReport
    Visible = False
    OnDblClick = lvDebugDblClick
    OnEnter = lvDebugEnter
  end
  object ilMenu: TImageList
    Left = 826
    Top = 4
    Bitmap = {
      494C01011C001D00040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000008000000001002000000000000080
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000CD6C0B00D27010009835000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000006529130067291300672913006729130067291300672913000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CC6802000000000000000000000000009C3B0C009C3B0C009C3B0C009C3B
      0C009C3B0C000000000000000000000000000000000000000000000000000000
      00000000000000000000CD6C0B00E7D7A5009835000000000000000000000000
      0000000000000000000000000000000000001583200015832000158320001583
      2000158320001583200015832000158320001583200015832000158320001583
      2000000000000000000000000000000000000000000000000000000000006829
      130067291300BB4B0000BB4B0000BB4B0000BB4B0000BB4B0000BB4B00006729
      1300672913000000000000000000000000000000000000000000000000000000
      0000CC680200CC680200CC680200CD6904009C3B0C00F89A9400EBA15E00DA84
      2C009C3B0C000000000000000000000000000000000000000000000000000000
      00000000000000000000CD6C0B00E7D7A5009835000000000000000000000000
      0000000000000000000000000000000000001583200015832000158320001583
      2000158320001583200015832000158320001583200015832000158320001583
      200000000000000000000000000000000000000000000000000083350B00B547
      0100BB4A0000BB4A0000BB4A0000BB4A0000BB4A0000BB4B0000BB4B0000BB4B
      0000B54701006729130000000000000000000000000000000000000000000000
      0000CC6802000000000000000000000000009B3A0C009C3B0C009C3A0C009B3A
      0C009C3B0C000000000000000000000000000000000000000000000000000000
      00000000000000000000CD6C0B00E7D7A5009835000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080340C00B5470000B848
      0000B6470000B6470000B6470000B6470000B8480000BA490000BB4B0000BB4B
      0000BB4B0000B547010067291300000000000000000000000000000000000000
      0000CC6802000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000CD6C0B00E7D7A5009835000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001B63
      AA00000000000000000000000000006600000066000000660000006600000066
      00000066000000000000000000000000000000000000A7430400B6470000B346
      0100B0450100BC550A00BF6F2C00D3844D00D3844D00C26D2700BA490000BB4B
      0000BB4B0000BB4B000068291300000000000000000000000000000000000000
      0000CC6802000000000000000000000000009C3B0C009C3B0C009C3B0C009C3B
      0C009C3B0C000000000000000000000000000000000000000000000000000000
      00000000000000000000CD6C0B00E7D7A5009835000000000000000000000000
      00000000000000000000000000000000000000000000000000001B63AA001B63
      AA000000000000000000000000000066000036B0650026A144001A942C00158B
      210000660000000000000000000000000000913A0800BB4C0100BB4D0200BB4D
      0200D3844D00FBFBFB00FBFBFB00FBFBFB00FBFBFB00E3B48700B6470000BA49
      0000BB4B0000BB4B0000BB4B0000652913000000000000000000000000000000
      0000CC680200CC680200CC680200CC6802009C3B0C00F89A9400EBA15E00DA84
      2C009C3B0C000000000000000000000000000000000000000000000000000000
      00000000000000000000CD6C0B00E7D7A5009835000000000000000000000000
      000000000000000000000000000000000000000000001B63AA005FBBBF005FBB
      BF001B63AA001B63AA0000000000006600000066000000660000006600000066
      000000660000000000000000000000000000973E0800BC550A00BC550A00BC58
      0D00FBFBFB00E2AE7D00BE611700BC580D00BC500500AF450200B2460100B848
      0000BB4A0000BB4B0000BB4B0000682913000000000000000000000000000000
      0000CC6802000000000000000000000000009B3A0C009C3B0C009C3A0C009B3A
      0C009C3B0C000000000000000000000000000000000000000000000000000000
      00000000000000000000CD6C0B00E7D7A5009835000000000000000000000000
      0000000000000000000000000000000000001B63AA00B6D8EA007FCAC4005FBB
      BF005FBBBF001B63AA0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000973E0800C26A2100BF651C00BF63
      1900FBFBFB00C26D2700BD5E1400BC550A00B2460100E3B48700B0450100B647
      0000BB4A0000BB4B0000BB4B0000682913000000000000000000000000000000
      0000CC6802000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000CD6C0B00E7D7A5009835000000000000000000000000
      000000000000000000000000000000000000000000001B63AA00C0D6E3005FBB
      BF001B63AA001B63AA0000000000006600000066000000660000006600000066
      000000660000006600000066000000660000973E0800D48A4C00BF6F2C00C06E
      2900FBFBFB00DEA16900C26A2100BE611700BC550A00FBFBFB00E2AE7D00B547
      0000BB4A0000BB4B0000BB4B0000682913000000000000000000000000000000
      0000CC6802000000000000000000000000009C3B0C009C3B0C009C3B0C009C3B
      0C009C3B0C000000000000000000000000000000000000000000000000000000
      00000000000000000000CD6C0B00E7D7A5009835000000000000000000000000
      00000000000000000000000000000000000000000000000000001B63AA001B63
      AA000000000000000000000000000066000036B0650036B0650032AE5C0032AE
      5C0026A14400209A3700168E2400056E0800953C0700DEA16900DB9B6100D384
      4D00EBC5A000FBFBFB00F3DEC900F2DBC400F5E7D900FBFBFB00FBFBFB00E9C0
      9900BB4A0000BB4B0000BB4B0000682913000000000000000000000000000000
      0000CC680200CC680200CC680200CC6802009C3B0C00F89A9400EBA15E00DA84
      2C009C3B0C000000000000000000000000000000000000000000000000000000
      00000000000000000000993F0800A28D75006C29070000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001B63
      AA00000000000000000000000000006600000066000000660000006600000066
      000000660000006600000066000000660000953C0700DD9E6500EBC5A000DB9B
      6100D8935600E3B18200EDCAA800EBC5A000F1D6BC00FBFBFB00FBFBFB00E6BB
      9100BB4A0000BB4B0000BB4B0000652913000000000000000000000000000000
      0000CC6802000000000000000000000000009B3A0C009C3B0C009C3A0C009B3A
      0C009C3B0C000000000000000000000000000000000000000000000000000000
      0000000000000166990001669900016699000166990001669900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009A3F0800EECDAD00EECD
      AD00E0A87300D8935600D3844D00BF6F2C00C1671F00FBFBFB00E1AC7900BB4B
      0000BB4A0000BB4B0000682913000000000000000000000000009C3B0C009C3B
      0C009C3B0C009C3B0C009C3B0C00000000000000000000000000000000000000
      0000000000000000000000000000000000009835000098350000983500009835
      00009835000001669900CBE0F9001798C6001798C600016699006C2907009835
      0000983500009835000098350000983500001583200015832000158320001583
      2000158320001583200015832000158320001583200015832000158320001583
      20000000000000000000000000000000000000000000953C0700DFA46D00F3DE
      C900F0D4B900E0A87300D3884C00BF6F2C00C26A2100E1AA7600BC580D00BC4E
      0300BB4B0000B5470100622814000000000000000000000000009C3B0C00F89A
      9400EBA15E00DA842C009C3B0C00000000000000000000000000000000000000
      000000000000000000000000000000000000D2701000E7D7A500E7D7A500E7D7
      A500E7D7A50001669900CFF0F800CBE0F9001798C60001669900A28D7500E7D7
      A500E7D7A500E7D7A500E7D7A500D27010001583200015832000158320001583
      2000158320001583200015832000158320001583200015832000158320001583
      2000000000000000000000000000000000000000000000000000953C0700DFA4
      6D00F1D6BC00F3DEC900EBC5A000DFA67000D8935600D48A4C00BF6F2C00BC54
      0800B5470100742F0F00000000000000000000000000000000009B3A0C009C3B
      0C009C3A0C009B3A0C009C3B0C00000000000000000000000000000000000000
      000000000000000000000000000000000000CD6C0B00CD6C0B00CD6C0B00CD6C
      0B00CD6C0B0001669900E7F0F300CFF0F800CBE0F90001669900993F0800CD6C
      0B00CD6C0B00CD6C0B00CD6C0B00CD6C0B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000973E
      0800A0410600E1AA7600E6BB9100E6BB9100E1AA7600D48A4C00BF6319009D40
      07006E2C11000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000166990001669900016699000166990001669900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009A3F08009D4007009D4007009D4007009D40070084350B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000A70D800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000064707A00BCA2A3000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A367
      6900A3676900A3676900A3676900A3676900A3676900A3676900A3676900A367
      6900A3676900A367690000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000076DD400197EE400076DD400076DD4001583200015832000158320001583
      2000158320001583200015832000158320001583200015832000158320001583
      2000000000000000000000000000000000006D8EC9001D55F30060758800C6A4
      9F00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B18C
      8400F1D7B900EECFAD00EECBA100EEC99600ECC58E00EAC18B00EAC18B00EAC1
      8B00EDC79100A36769000000000000000000000000000000000018CEF60019CB
      F70000000000000000000000000000000000000000000000000000000000076D
      D4002A94F3002BACF900076DD400000000001583200015832000158320001583
      2000158320001583200015832000158320001583200015832000158320001583
      2000000000000000000000000000000000006FB6F3006FB6F3001D55F3006075
      8800C6A49F000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B190
      8900F2D8BC00F0D4B600EDCDA800EECBA100EDC79100EBC38C00E9BF8800E9BF
      8800EBC38C00A3676900000000000000000000000000000000000000000018CE
      F60019CBF7000000000000000000076DD400076DD400076DD400076DD4002790
      F0002CA4F900076DD40000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000006FB6F3006FB6F3001D55
      F30060758800CBA69D0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B292
      8B00F4DEC600F2D8BC00F0D4B600EECFAD00EECBA100EEC99600EBC38C00E9BF
      8800EBC38C00A367690000000000000000000000000000000000000000000000
      000000000000000000000A70D8000E73DB002790F0004FCBF3004FCBF300238B
      EE00076DD40000000000000000000000000000000000000000002066AB000000
      0000000000000000000000000000006600000066000000660000006600000066
      00000066000000000000000000000000000000000000000000006FB6F3006FB6
      F3001D55F30060758800BCA2A300000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B19D
      9700F9EDDF00F7E5D000F4DEC600F2D8BC00F0D4B600EECFAD00EECBA100EBC3
      8C00ECC58E00A367690000000000000000000000000000000000000000000000
      00000000000010635D001176DE002CA4F9001F86E9002E9EF8002BACF9002E9E
      F800076DD40000000000000000000000000000000000000000002066AB002066
      AB000000000000000000000000000066000044B65D0030AB47001E9E2F001593
      2300006600000000000000000000000000000000000000000000000000006FB6
      F3006FB6F3006A93D500656F77000000000089878900C6A49F00D3B4A800CDA6
      9D0000000000000000000000000000000000000000000000000000000000AFA1
      9C00FAF2E800F8E8D600F7E5D000F4DEC600F2D8BC00F0D4B600EDCDA800EDC7
      9100EDC79100A367690000000000000000000000000000000000000000000000
      00000B8813001A9A2A0010635D002790F0004FCBF300197EE4001F86E9002D98
      F600076DD4000000000000000000000000002066AB002066AB004393D8004393
      D8002066AB000000000000000000006600000066000000660000006600000066
      0000006600000000000000000000000000000000000000000000000000000000
      00006FB6F3009EBBE10089878900BCA2A300EDE7D300FAF4D400FAF4D400FAF4
      D400E2DACC00D2AEA2000000000000000000000000000000000000000000E2AC
      9A00FCF8F300FAF2E800F8E8D600F7E5D000F4DEC600F2D8BC00F0D4B600EECB
      A100EEC99600A367690000000000000000000000000000000000000000000674
      080012911E004EB47A002EBB520010635D001F86E9002E9EF8001176DE001C82
      E700076DD4000000000000000000000000002066AB00A9C8B40092C79F004393
      D8004393D8002066AB0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D5B9AD00D5B9AD00FCEFC500FAF4D4009EBBE10066A3E700EDE7
      D300F9F5DB00F6F2D9008987890000000000000000000000000000000000E2AC
      9A00FDFCFB00FBF5EF00FAF2E800F8E8D600F7E5D000F4DEC600F2D8BC00EDCD
      A800EECBA100A367690000000000000000000000000000000000000000000A79
      0D0033C25D001FA3340037C2660012911E0011635C00197EE4002D98F6000E73
      DB00076DD4000000000000000000000000002066AB002066AB004393D80074C1
      86002066AB000000000000000000006600000066000000660000006600000066
      0000006600000066000000660000006600000000000000000000000000000000
      000000000000C6A49F00FCE6C200FCE6C200FCF4CB0088B9EC000734FB00DDD5
      CC00F9F5DB00F9F5DB00D8CCC50000000000000000000000000000000000E2AC
      9A00FDFCFB00FDFCFB00FCF8F300FAF2E800F8E8D600F7E5D000F4DEC600F1D7
      B900EDCCA500A367690000000000000000000000000000000000000000000A79
      0D0033C25D001C9D2E000B881300057F08000279030011635C001176DE00076D
      D4000000000000000000000000000000000000000000000000002066AB002066
      AB000000000000000000000000000066000050B9670044B65D0044B65D0039B0
      510029A63F001E9E2F001797260007700B000000000000000000000000000000
      000000000000D5B9AD00FCECC100B6A9B3006998D8003849F5000734FB0066A3
      E7007FB9F000E8DFCE00F3EED600C6A49F00000000000000000000000000E3B2
      9600FDFCFB00FDFCFB00FDFCFB00FCF8F300FAF2E800F8E8D600F7E5D000EED0
      B200B29B9300A367690000000000000000000000000000000000000000000A79
      0D002AB749001C9D2E0006810B00037B0500027903000279030011635C000000
      00000000000000000000000000000000000000000000000000002066AB000000
      0000000000000000000000000000006600000066000000660000006600000066
      0000006600000066000000660000006600000000000000000000000000000000
      000000000000D7BFB200FCECC1006C91D0000734FB000734FB000734FB000734
      FB000734FB009EBBE100FAF4D400C6A49F00000000000000000000000000E3B2
      9600FDFCFB00FDFCFB00FDFCFB00FDFCFB00FBF5EF00FAF2E800F4DEC600A370
      6F00A3706F00A367690000000000000000000000000000000000000000001291
      1E000E8B170009851000037B0500037B0500016E0100016C01000000000018CE
      F60019CBF7000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D7BFB200FCECC100CEA79D00B6A9B30064A1E2000734FB0096BB
      E600DAD1CB00F6F2D900F6F2D900C6A49F00000000000000000000000000EDCC
      A500FDFCFB00FDFCFB00FDFCFB00FDFCFB00FDFCFB00FDFCFB00C4BDBC00E9BF
      8800BF774F0000000000000000000000000000000000000000001C9D2E0033C2
      5D00067408000169020001690200016902000169020000000000000000000000
      000018CEF60019CBF70000000000000000001583200015832000158320001583
      2000158320001583200015832000158320001583200015832000158320001583
      2000000000000000000000000000000000000000000000000000000000000000
      000000000000CBA69D00FCEFC500FCECC100FCE6C2006F88C1000734FB00DAD1
      CB00FAF4D400F9F5DB00E2DACC0000000000000000000000000000000000E5BB
      8E00FBF5EF00FBF5EF00FBF5EF00FBF5EF00FBF5EF00FBF5EF00C4BDBC00B585
      74000000000000000000000000000000000000000000137C160033C25D000985
      1000016902000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001583200015832000158320001583
      2000158320001583200015832000158320001583200015832000158320001583
      2000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000D7BFB200F9F5DB00F9F5DB00D3B4A800BCA2A300D7BF
      B200FCE6C200FCF4CB00BCA2A30000000000000000000000000000000000EECF
      AD00EED0B200EED0B200EED0B200EED0B200EED0B200EED0B200EED0B2000000
      000000000000000000000000000000000000026B0300036F0500027702000169
      0200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000D7BFB200F6F2D900FCECC100FCECC100FCEC
      C100FCDBC700D0ABA00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000015952200026B0300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CBA69D00D0ABA000D7BFB200D5B9
      AD00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE00000000000000000000000000078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00000000000000000000000000000000000000000000000000189D
      C60048BDDE0060C6E10051C1E0003CB9DC004DBFDF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000095636300505C5200505C5200000000000000
      000000000000000000000000000000000000078DBE0062CBF200078DBE00A4F6
      FD0066CEF50066CEF50066CEF50066CEF50066CEF50066CEF50066CEF50066CE
      F5003AAFDA00ABF2FC00078DBE0000000000078DBE0025A0CD005FC8EE0083E1
      FB0066CDF40066CDF40066CDF40066CDF40066CDF40066CDF40066CDF40066CD
      F4003AAED8001495C400000000000000000000000000000000001EA2CA0022A5
      CE007ED2EC00A2E1F90098DCF80091DBF7007BD1EB0075CFE80048BDDE000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009863630098636300505C5200A9A2A600505C5200505C
      520000000000000000000000000000000000078DBE006BD2F700078DBE00A9F3
      FC006FD4F8006FD4F8006FD4F8006FD4F8006FD4F8006FD4F8006FD4F8006FD4
      F8003AAFDA00BEECF400078DBE0000000000078DBE004CBBE30031A8D30095EC
      FB006ED4F9006ED4F9006ED4F9006ED4F9006ED4F9006ED4F9006ED4F9006ED4
      F9003FB1DB00C8F6FB00078DBE0000000000000000000000000022A5CE002DAD
      D40086D6F000A2E1F90091DBF70082D3ED0059C5E10091DBF70098DCF80075CF
      E800000000000000000000000000000000000000000000000000000000000000
      00000000000098636300A8737400CF747500505C5200DDC8C900D5C8CA00A89F
      A100505C5200505C52000000000000000000078DBE0072D6FA00078DBE00ABF2
      FC0079DCFA0079DCFA0079DCFA0079DCFA0079DCFA007ADDFB0079DCFA0079DC
      FA0045B6DF00BEECF400078DBE0000000000078DBE0072D6F900078DBE00ACF8
      FD007ADBFA007ADBFA007ADBFA007ADBFA007ADBFA007ADBFA007ADBFA007ADB
      FA0043B5DD00C8F6FB00078DBE000000000000000000000000002DADD40037B7
      DB0094DCF700B3E9FB00A2E1F90060C6E100169AC4006ECBE50094DCF70098DC
      F80034B1D7000000000000000000000000000000000000000000000000000000
      000098636300A8737400D0949500CD7D7E00505C5200C4C1C500D5C8CA00DDC8
      C900D5C8CA00BBBDC200505C5200505C5200078DBE007ADDFB00078DBE00B4F3
      FC0084E4FB0084E4FB0084E4FB0084E4FB0084E4FB0084E4FB0084E4FB0084E4
      FB0045B6DF00C3F0F700078DBE0000000000078DBE007CDDFA001495C40095EC
      FB0092EAFB0086E3FB0083E1FB0083E1FB0086E3FB0083E1FB0083E1FB0086E3
      FB0049B8E000C8F6FB001495C40000000000000000000000000036B5DA0041BA
      DD00B1E5F900B8EBFB00B6EBFB0082D3ED001496C00065C8E20094DCF70094DC
      F70034B1D7000000000000000000000000000000000000000000000000009863
      6300B47C7D00E5A6A600C98E9000B8878C009F696A00505C5200505C5200BBBD
      C200D5C8CA00D5C8CA00ADB3BC006E676700078DBE0081E2FB00078DBE00BEF4
      FC008EECFC008EECFC008EECFC008EECFC008EECFC008EECFC008EECFC008EEC
      FC004CBCE400BEF4FC00078DBE0000000000078DBE0083E1FB0043B5DD0059C4
      EA00ACF8FD008FE9FB008FE9FB008FE9FB008FE9FB008FE9FB008FE9FB008FE9
      FB004CBBE300C8F6FB00C8F6FB00078DBE0000000000000000003CB9DC0048BD
      DE00B8EBFA00B8EBFB00B8EBFB00B3E9FB0060C6E10082D3ED009CDEF80094DC
      F70034B1D700000000000000000000000000000000000000000098636300B47C
      7D00F6B9B900DA9D9E00BC848800C2888900C2888900D0797B0099636300505C
      5200505C5200BBBDC200A8A7AD006E676700078DBE008BEAFC00078DBE00D2F7
      FC00C8F6FC00C8F6FC00C8F6FC00C8F6FC00C8F6FC00C8F6FC00C8F6FC00C8F6
      FC00BADADF00D2F7FC00078DBE0000000000078DBE008CE7FB0078DAFA0025A0
      CD00E5F8FA00C8F6FB00C8F6FB00C8F6FB00C8F6FB00C8F6FB00C8F6FB00C8F6
      FB0095ECFB00E5F8FA00CFF6FA00078DBE00000000000000000034B3D80048BD
      DE0060C6E10065C8E20065C8E20060C6E1006ECBE50086D6F000A2E1F900A2E1
      F90034B1D7000000000000000000000000000000000098636300BC848800F1C5
      C500E5A6A600A8737400C2888900D69A9B00D69A9B00C8808300D1787900CD6D
      6E0099636300505C5200505C52006E676700078DBE0094F1FD00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE0000000000078DBE0096F0FC0096F0FC001495
      C400078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00000000000000000034B1D70059C5
      E10041BADD0048BDDE0048BDDE0037B7DB004DBFDF0075CFE8009CDEF80086D6
      F00028A8CF000000000000000000000000000000000098636300F1C5C500F0AF
      AE00A8737400C98E9000E5A6A600AAAFB70017C5F20087A1B300D1787900CD6F
      7000CD6D6E00CE6676009963630098636300078DBE009BF4FD009BF4FD009BF4
      FD009BF4FD009BF4FD009BF4FD009BF4FD009BF4FD009BF4FD009BF4FD009BF4
      FD00078CBD00000000000000000000000000078DBE009CF5FD009AF4FD009AF4
      FD009CF5FD009DF6FD009AF4FD009CF5FD009AF4FD009CF5FD009AF4FD009AF4
      FD00088DBE00000000000000000000000000000000000000000034B1D70060C6
      E100169AC4003CB9DC008FDCF60059C5E10036B5DA00189DC6002CABD10051C1
      E00022A5CE000000000000000000000000000000000098636300F3B2B200A873
      7400C98E9000F5B7B700B0B6BE0017C5F20017C5F200A9A2A600CF7C7D00CF74
      7500CD6D6E00CE6676009863630000000000078DBE00D2F7FC00A0F6FD00A0F6
      FD00A0F6FD00A2F7FD00A2F7FD00A0F6FD00A2F7FD00A2F7FD00A0F6FD00A2F7
      FD00078CBD00000000000000000000000000078DBE00E5F8FA00A1F9FE00A1F9
      FE00A1F9FE00A1F9FE00A1F9FE00A1F9FE00A1F9FE00A1F9FE00A1F9FE00A1F9
      FE00088DBE000000000000000000000000000000000000000000000000001196
      C0001196C0002DADD4008FDCF6008FDCF60048BDDE001196C000169AC40022A5
      CE00000000000000000000000000000000000000000098636300B8878C00DA9D
      9E00F1C5C500F4BFC00017C5F20017C5F20078A7BD0017C5F200C98E9000D178
      7900CD6D6E0098636300000000000000000000000000078DBE00D2F7FC00A4F6
      FD00A4F6FD00A4F6FD00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE000000000000000000000000000000000000000000078DBE00E5F8FA00A4F9
      FE00A4F9FE00A4F9FE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00000000000000000000000000000000000000000000000000000000000000
      0000169AC400169AC400000000003CB9DC0022A5CE001196C0001196C0000000
      000000000000000000000000000000000000000000009F696A0098636300F6B9
      B900F1C5C500F3C2C200F5B6B500B0B6BE0017C5F200A8A7AD00C2888900D079
      7B00986363000000000000000000000000000000000000000000078DBE00078D
      BE00078DBE00078DBE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000078DBE00078D
      BE00078DBE00078DBE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000051C1E000189DC6000000000000000000000000001196C0001196C0000000
      00000000000000000000000000000000000000000000248531003CAD59009863
      630098636300F6B9B900F6BDBD00F3B2B200E8A9A900DA9D9E00C88083009863
      6300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000028A8CF007BD1EB002CABD100000000001496C000189DC600000000000000
      000000000000000000000000000000000000248531003FBF62003FBF620034A8
      4D002D9B42007B61610098636300DD9FA000E8A9A900C98E9000986363000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000048BDDE007BD1EB006ECBE50048BDDE00169AC400000000000000
      00000000000000000000000000000000000024853100248531003FBF62003AB6
      5800248531000000000000000000986363009863630098636300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000248531002485
      3100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000066676900BE988E000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000159F
      C90046B9DA0062C1DE0055C1DE003DB5DA004BBEDC0000000000000000000000
      000000000000000000000000000000000000509AD3004596E7006C757E00C49C
      9100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000854302008340
      0000733900006B35000068340000683400006834000068340000683400006834
      0000683400005D2F030000000000000000000000000000000000854100008541
      0000763A00006B350000673301006B3500006B3500006B3500006B3500006B35
      00006B3500005B2E03000000000000000000000000000000000016A4CD0016A4
      CD007DD4EE009EE1F90097E0F80092E0F7007DD4EE0072CDE80046B9DA000000
      0000000000000000000000000000000000004FA7EA004FA7EA004596E7006C75
      7E00C49C91000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AB540000D0670000BC5D
      0000BC5D0000B5590000B0570000AE550000AE550000AE550000AE550000AE55
      0000B5590000964A00005D2F03000000000000000000AA540000CF660000CF66
      0000B6590000B5590000AF560000AD550000AF560000AF560000AF560000AF56
      0000B4590000964A00005B2E03000000000000000000000000001BA8D10027AF
      D70081D7F000A1E3F90092E0F70081D7F00059C1DE008FE0F6009BE0F80076CF
      E90000000000000000000000000000000000000000004FA7EA004FA7EA004596
      E7006C757E00C49C910000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D0670000E7730100DA6E
      0300D46A0100D0670000D0670000BC5D0000BC5D0000BC5D0000BC5D0000BC5D
      0000BC5D0000B5590000683400000000000000000000CF660000E7740300DC6D
      0100D66A0000D0670000CF660000B6590000B6590000B6590000B6590000B659
      0000CF660000B45900006B35000000000000000000000000000027AFD70030B2
      D90095E0F700AFE8F900A6E5FA0062C1DE00179BC3006FCBE60095E0F7009BE0
      F80033B3DA0000000000000000000000000000000000000000004FA7EA004FA7
      EA004596E7006C757E00B8948A00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D1680100EB760400E372
      0300DA6E0300EC862100E9974600F0A35800E9974600E8934000BA5C0000BA5C
      0000BC5D0000AE550000683400000000000000000000D1670100EA760500E271
      0200DC6D0100D4690000E6934100E69A4F00E69A4F00E6974900D7611900B659
      0000B6590000AF5600006B35000000000000000000000000000030B2D9004BBE
      DC00ACE4F800AFE8F900AFE8F9006CC9E4001895BC0062C1DE0095E0F70095E0
      F70033B3DA000000000000000000000000000000000000000000000000004FA7
      EA004FA7EA004596E700676A6C0000000000B08D8700C6A09300C6A09300BD98
      8E000000000000000000000000000000000000000000D46A0100EE7F1300ED7A
      0A00F0A35800FCF8F500FCF8F500FCF8F500FCF8F500F8CA9E00BC5D0000BA5C
      0000BC5D0000AE550000683400000000000000000000D66A0000EA7E1400ED7A
      0800E7740300DF6F0100F8C99C00FCFAF800FCFAF800FCFAF800FCFAF800E69A
      4F00B6590000AF5600006B35000000000000000000000000000037B4DA004BBE
      DC00AFE8F900AFE8F900AFE8F900AFE8F90062C1DE0076CFE90097E0F80095E0
      F70033B3DA000000000000000000000000000000000000000000000000000000
      00004FA7EA00F1DBEA00C49C9100BB968C00F5E1B900FEFDE500FEFDE500FDF9
      D900DEBE9E00B38D8600000000000000000000000000D46A0100EC8D3100EE82
      1800FCF8F500F7C08B00ED841D00EE821800DA641600D5690400BC5D0000BC5D
      0000BC5D0000AE550000683400000000000000000000D66A0000E98C3100EB81
      1900ED7A0800E7740300E5730300EA7E1400D7611900D7611900F4BF8C00FCFA
      F800B6590000AF5600006B35000000000000000000000000000030B2D9004BBE
      DC0062C1DE006CC9E40066C6E20066C6E2006FCBE60084DAF2009EE1F900A1E3
      F90033B3DA000000000000000000000000000000000000000000000000000000
      00000000000000000000CDA89600F7E7C000FDF9D900FDFBDC00FDFBDC00FEFD
      E500FDFDEA00F1DBEA00B08D87000000000000000000D46A0100F2A55B00ED8A
      2900FCF8F500ED841D00E9750300E3720300DF700300F8CA9E00D0670000D067
      0000BC5D0000B0570000683400000000000000000000D66A0000EDA45C00EA8A
      2C00EA7E1400ED7A0800F8C99C00E2710200DF6F0100D66A0000D4670A00FCFA
      F800CF660000B15700006B3500000000000000000000000000002BB1D80076CF
      E9001895BC001897BE0081D7F0004BBEDC0027AFD7001BA8D1002BB1D80079D2
      EC0037B4DA000000000000000000000000000000000000000000000000000000
      00000000000000000000C6A09300F5DBAE00FBF4D300FDFBDC00FEFDE500FDFD
      EA00FDFDEA00FDFDEA00CDA896000000000000000000D46A0100F6B37200EB90
      3700FCF8F500F5AA6200EC780600EB760400E7730100FCF8F500F8C59400D168
      0100D0670000B85B0000733900000000000000000000D66A0000F4B27200E890
      3900EB811900F8C89900FCFAF800EA760500E5730300DF6F0100E89E5500FCFA
      F800CF660000B6590000723800000000000000000000000000000000000030B2
      D9004BBEDC0076CFE90097E0F80089DDF40051C0DE00159FC9001597BF0016A4
      CD0016A4CD000000000000000000000000000000000000000000000000000000
      000000000000C6A09300F5DDB000E6C59E00F9EECB00FEFDE000FEFDE500FDFD
      EA00FDFDEA00FDFDEA00F9EECB00B8948A0000000000D46A0100F6B97E00E893
      4000F8CA9E00FCF8F500FBE3CB00FBE3CB00FBEBDA00FCF8F500FCF8F500F9D5
      B100D66B0200BC5D0000834000000000000000000000D66A0000F5B97D00E693
      4100FADCBE00FCFAF800FCFAF800FBE8D500FAE1C700FAE1C700FCFAF800F4BF
      8C00D96C0000CF66000085410000000000000000000000000000000000000000
      0000000000000000000055C1DE004BBEDC004BBEDC00179BC3001597BF0016A4
      CD0016A4CD000000000000000000000000000000000000000000000000000000
      000000000000C6A09300F5DDB000D9B89B00F5DBAE00FDF9D900FEFDE000FEFD
      E500FEFDE500FEFDE000F9EECB00BB968C0000000000D46A0100F7BE8700ED9C
      4C00EB903700F6B37200F8CA9E00F8CA9E00FADEC300FCF8F500FCF8F500F9D0
      A800E3720300D0670000914800000000000000000000D66A0000F4BC8500E89E
      5500F9D8B700FCFAF800FCFAF800FAE1C700F8C99C00F8C99C00EDA45C00E271
      0200E2710200CF66000090470000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000179BC3001597BF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BB968C00F7E7C000E6C59E00E9CAA200F5E1B900FDF9D900FEFD
      E000FDFBDC00FEFDE000F5E1B900B48E870000000000D46A0100F8C59400F5AA
      6200E9923C00EB903700ED882500ED841D00EE821800FCF8F500F7C08B00EB76
      0400E9750300D46A01009D4D00000000000000000000D66A0000F8C79600EEA7
      6100E8903900F8CCA100FCFAF800EA831D00EB811900ED7A0800EC780600EA76
      0500EA760500D66A00009E4E0000000000000000000000000000000000000000
      00001895BC001895BC00000000000000000000000000179BC3001895BC000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000DEBE9E00FEFDE500F5E1B900E9CAA200F3D5A600F7E7
      C000FBF4D300FDF9D900B69087000000000000000000D46A0100F8C79800F8C3
      8E00F7B67800F6B06C00F2A55B00ED9C4C00EB903700F7C08B00ED7A0A00EC78
      0600EC780600DF700300AB5400000000000000000000D66A0000F8C89900F5C2
      9000F5B97D00F1AC6800F9D3AE00E6974900E98C3100EB811900ED7A0800EC78
      0600EC780600E2710200AA540000000000000000000000000000000000000000
      000030B2D900159FC900000000000000000000000000159FC9001895BC000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B38D8600F1DBEA00FDFDEA00F3D5A600E6C59E00F2D2
      A300F7E7C000D5B39900B69087000000000000000000D46A0100F7B67800F8CA
      9E00F8CA9E00F8C59400F7BC8300F6B06C00ED9C4C00ED882500EE7D0E00EC78
      0600EC780600E9750300B55900000000000000000000D66A0000F5B97D00F8C9
      9C00F8CCA100F8C79600F4BC8500F4B27200E6974900EA882800ED7A0800ED7A
      0800ED7A0800E7740300B5590000000000000000000000000000000000000000
      000046B9DA0072CDE800000000000000000000000000179BC3001597BF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000B08D8700CDA89600F5E1B900F5DEB300ECCD
      A300B6908700B690870000000000000000000000000000000000DA641600EC8D
      3100EC8D3100EC8D3100ED8A2900EC862100DA641600D8670C00D66B0200D66B
      0200D66B0200D067000000000000000000000000000000000000EA7E1400E98C
      3100E98C3100E98C3100EA8A2C00EA852200EA7E1400D4670A00D96C0000D66A
      0000D66A0000CF66000000000000000000000000000000000000000000000000
      00000000000059C1DE0079D2EC0046B9DA003DB5DA001BA8D100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BA958B00BB968C00B691
      8800000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000055C1DE0055C1DE0046B9DA0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000006E3931006E382F0082422500824225006E382F006E3931000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000001079F000313AA000319B0000319B0000313AA000108A1000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000868483008684830000000000000000008684830086848300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000001EA1CD002CAAD3001585B000000000000000
      000000000000000000000000000000000000000000000000000000000000723A
      2D007F402600AF591100C2610600CB660100CB660100C4620400B15A10008242
      25006E3931000000000000000000000000000000000000000000000000000106
      9F000319B0000530C2000733C6000733C6000733C6000733C6000531C2000420
      B60001069F000000000000000000000000000000000000000000000000008684
      8300C0BAB800B8B3B300868483008684830086848300EDEBE900868483008684
      83008684830000000000000000000000000000000000000000000000000027A5
      CF0023A4CE001585B0001E98C2002CABD3002CABD300178AB5001587B1002CAB
      D30029A7D10000000000000000000000000000000000000000007C3F2800A855
      1400D0690300D0690300CE680200CA650100CD670100CD670100D0690300D069
      0300A85514006E393100000000000000000000000000000000000108A100042C
      C0000733C6000733C6000531C2000531C2000531C2000531C2000733C6000733
      C600042CC000020CA5000000000000000000000000000000000086848300E3E2
      E100B8B3B300B8B3B300B8B3B300505050004F4F4F0086848300CECCCC00E9E7
      E600C8C7C70086848300868483008684830000000000000000000000000027A5
      CF002CABD3002CABD3001DA0CC0020D0F8004AD7F70027A5CF005EC1DC0064C1
      DA0027A5CF000000000000000000000000000000000082422500AF591100D069
      0300CA650100CF711500E7BF9500DD8C3A00C7620300CA650100CB660100CB66
      0100D0690300A85514006E393100000000000000000001069F000225CC000C38
      D3000531C3000223C900042EC1000531C2000531C2000530C200042ABE00042E
      C1000733C600042EC10001069F00000000000000000086848300E0DEDE00D8D5
      D500A9A4A300A9A4A300A9A4A300595858001616160012121200181818001212
      120093888500CBCAC900868483000000000000000000178CB6001E98C2004FC2
      E1007FDFF5004FC2E1003ACBEF001DD1FA003CD5F8005DD9F60074C1D600AEE2
      ED0091E3F500178AB500000000000000000000000000974D1C00D0690300CE68
      0200CA650100CF6E0F00F8E5D200FCF2E900DE924500C7620300CA650100CB66
      0100CB660100D06903008242250000000000000000000219B9000836ED00022C
      DE00133BD8005070F1001C3FC800042CC000042CC0000A34C7005070F1001C3F
      C800042EC1000733C6000420B6000000000086848300D8D5D500D8D5D500A9A4
      A300E0DEDE00DAD8D800CDCBCB00C5BDB900B8B3B300A9A3A100777676001212
      120014141400131313008684830093888500000000005EC1DC002CABD3004FC2
      E10096E4F5008BE2F5005DD9F60027D0F7001DD1FA0056D9F70076DEF5007FDF
      F5003ACBEF002CABD30027A5CF0000000000A8551400BF600800D06D0B00D072
      1700CF701300D2751900F6E0C900FEFDFD00FDF6F000E3A56700CA650100C964
      0200CB660100D0690300B35B0E006E39310001069F00022EE6000635F200022D
      E3005070F100FBFBFE00B8C6FB000B35CA00042EC100A1B3F700FBFBFE005070
      F100042ABE000733C6000531C2000108A10086848300D8D5D500A9A4A300F4F1
      ED00F6F3EF00F6F3EF00E9E7E600E9E7E600E5E4E300DAD8D800CCCBCB00C0BA
      B800A9A3A10072717100938885008E868300000000001B91BC0023A4CE004FC2
      E1007FDFF50091E3F50070DCF5005EC1DC004FC2E1002DCFF4001DD1FA001DD1
      FA001DD1FA0020D0F8002CABD30000000000AF591100D2751900D67E2600D984
      2D00D9842D00D9842D00F7E2CC00FEFDFD00FEFDFD00FEFDFD00E7BF9500CF6E
      0F00C9640200CE680200C46204006E393100020FAB000333F6000333F6000230
      EE000836ED0096A2FA00FBFBFE00B2C1FA00A7B9F700FBFBFE00A7B9F7001C3F
      C800042EC1000531C2000733C6000313AA0086848300A9A4A300F6F3EF00F6F3
      EF00F6F3EF00E0DEDE00ADA9A900B1ACAC00C5BDB900D1CFCE00E0DEDE00E3E2
      E100D4D2D100C8C7C700AAA5A50085838300000000001A8FBA002CABD30045C7
      E9006BDBF60089BECD00979696009796960097969600979696004FC2E1001DD1
      FA001DD1FA001DD1FA001D9EC90000000000B55D0E00DD8A3700DE924500DD8F
      4000DD8F4000DE924500F8E5D200FEFDFD00FEFDFD00FEFDFD00FEFDFD00F5D8
      BC00D0721700CA650100CD6701006E382F000219B9001A44E7000635F2000434
      F5000230F000022EE60096A5FA00FBFBFE00FBFBFE00A7B9F7000632C500042C
      C0000531C2000531C2000733C6000319B000000000008684830086848300F2EF
      EC00C0BAB80093888500A9A3A100ADA9A900ACA7A700B1ACAC00ADA9A900B1AC
      AC00CAC9C900DDDBDB00D1CFCE0086848300000000001B91BC002CABD3002CAB
      D3005DD9F60097969600E2DFE300B0B4B700ACA3A200BEB2B2009796960056D9
      F7009DE3F200AEE2ED001E98C20000000000B85E0C00E4A25F00E3A56700E49F
      5900E49F5900E49F5900F9E9D900FEFDFD00FEFDFD00FEFDFD00FEFDFD00F1CD
      A900CF711500CA650100CD670100723A2D00031DBE004467F7001F49EB000333
      F6000231F3000231F200A4B6F700FBFBFE00FBFBFE00B8C6FB000C36CE00042C
      C0000531C2000531C2000733C6000319B0000000000000000000000000008684
      8300C99B9700BF9B8B009F8D8500908684008280800093888500A9A4A300BBB6
      B500D8D5D500D1CFCE00868483000000000000000000178CB6001EA1CD002CAB
      D30045C7E90097969600E2DFE300B0B4B700ACA3A200BEB2B200979696007FDF
      F500AEE2ED0089BECD0027A5CF0000000000B15A1000E3A56700F0CBA500E3A5
      6700E3A56700E3A56700FAECDD00FEFDFD00FEFDFD00FEFDFD00EDBE8D00C762
      0300C9640200CE680200C46204006E3931000215B3005070F1004A6CF6000333
      F6000635F200A1B3F700FBFBFE00A4B6F70096A5FA00FBFBFE00B8C6FB001C3F
      C800042EC1000632C5000733C6000313AA000000000000000000000000000000
      0000C99B9800FBE4C900F9DAB700F0D4B600C69C9100C69C9100C99B98009A8B
      85008684830086848300000000000000000000000000000000001585B00021A3
      CE003ACBEF0097969600E2DFE300B0B4B700ACA3A200BEB2B2009796960067DA
      F60064C1DA001585B0000000000000000000B55D0E00E0974D00F6DCC300F1CD
      A900EEBE8D00EEBE8D00FBEEE100FEFDFD00FCF2E900E3A56700CF6A0400CA65
      0100CB660100D0690300B15A10006E39310001069F004A6AEE0096A5FA001F49
      EB004C6EF500FBFBFE00A7B9F7000333F6000230EC0096A2FA00FBFBFE005070
      F1000226D0000B35CA000530C20001069F000000000000000000000000000000
      0000C99B9800FBE4C900F9DBB900F9DAB700F9DAB700F6D8B600C99B98000000
      0000000000000000000000000000000000000000000000000000000000001589
      B4001A8FBA0097969600E2DFE300B0B4B700ACA3A200BEB2B200979696001E98
      C2001B91BC0000000000000000000000000000000000CF711500F1CDA900FBEE
      E100F3D4B300EEBF8E00FCF2E900FBEFE300E4A25F00D57A2000D2751900CB66
      0100CA650100D06903007F40260000000000000000001C3FC8009EAFF90096A2
      FA002850EF004C6EF5000635F2000231F3000231F3000635F2004C6DF1000C38
      E500032CDB000C38D3000420B60000000000000000000000000000000000C99B
      9800FCECD900FCE7CF00F9E0C300F9DBB900F9DAB700C99B9800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000097969600E2DFE300BAAFAE00AAA1A100BEB2B200979696000000
      00000000000000000000000000000000000000000000B85E0C00DE924500F7E2
      CC00FBF1E600F4D6B700F5D8BC00E3A56700DC893500D9842D00D2751900D06B
      0700D0690300A252160070392E00000000000000000001069F004462E100B8C6
      FB009EAFF9003B60F8000635F2000333F6000333F6000333F6000231F2000836
      ED000836ED000225CC0001069F0000000000000000000000000000000000C99B
      9800FAEFE200FCECD900FBE5CC00F9E0C300F9DAB700C99B9800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000097969600B0B4B700A6A0A00098969600A8A0A000979696000000
      0000000000000000000000000000000000000000000000000000B35B0E00DE92
      4500F4D6B700FBEEE100F7E2CC00F0CBA500EDBE8D00E3A56700E29C5400D57A
      2000A45315007C3F280000000000000000000000000000000000010AA4004462
      E100A7B9F700C0CCFB0096A5FA005070F1005070F1004A6CF6003B60F8001A44
      E700032AD8000108A10000000000000000000000000000000000C99B9800F6F3
      EF00F6F3EF00FAEFE200FCEAD600FCE7CF00F3D6B700C99B9800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000097969600E0E0E300BEB2B200A6A0A000B0A5A400979696000000
      000000000000000000000000000000000000000000000000000000000000B35B
      0E00CF711500DAA16700EABE9200F0CBA500EEC29400E0A46700D67E2600974D
      1C007C3F28000000000000000000000000000000000000000000000000000106
      9F001C3FC8005070F100999FFA0096A2FA00A096FB005070F1001F49EB00041F
      C10001069F000000000000000000000000000000000000000000C99B9800C99B
      9800C99B9800F8F2EA00FCECD900FCE7CF00C99B980000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000097969600E2DFE300E2DFE300B9B3B400A39E9E00979696000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AF591100B55D0E00B15A1000A8551400AC581200A45315000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000001069F000219B9000321C600041FC1000215B30001069F000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C99B9800C99B9800C99B98000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000097969600979696009796960097969600000000000000
      0000000000000000000000000000000000000000000000000000B7818200B781
      8200B7818200B7818200B7818200B7818200B7818200B7818200B7818200B781
      8200B7818200B7818200B78182000000000000000000078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C8A79D00F8EA
      D900F4E0C800F2DABD00F2D8B300F2D4AA00F1D3A600EFCFA200EFCFA200EFCF
      A200EFCFA200F0D1A100B781820000000000078DBE001A9DAA005EC7EB0084E1
      FA0066CDF20066CDF20066CDF20066CDF20066CDF20066CDF20066CDF20066CD
      F20046B8D400078DBE0000000000000000000000000000000000006600000066
      0000AAAC9C00AAAC9C00AAAC9C00AAAC9C00AAAC9C00AAAC9C00AAAC9C000066
      000000660000000000000000000000000000000000000000000097433F009743
      3F00C2999900C2999900C2999900C2999900C2999900C2999900C29999009230
      2F0097433F000000000000000000000000000000000000000000C8A79D00F8EA
      D900F5E4CF00F3DEC500F2D9B900F2D8B300F2D4AA00F0D1A100EECEA200EECE
      A200EECEA200F0D1A100B781820000000000078DBE004BBBDD0046B8D4009BF1
      FC0072D6F80072D6F8006DD2F60072D6F80072D6F80072D6F80072D6F8006DD2
      F60048B9D90080DEF900078DBE00000000000000000000660000009800000098
      0000E5E1E5000066000000660000E7E6E700E5E4E600E5E1E500CCCCCC000066
      0000007D00000066000000000000000000000000000097433F00CD666600C663
      6300E4E0E400922B2B00922B2B00E6E5E700E5E3E500E4E0E400CECACC00922B
      2B009E43410097433F0000000000000000000000000000000000C9A99E00FAF1
      E800F7E7D500F5E4CF00F3DEC500F2DABD00F2D8B300F2D4AA00F0D1A100EECE
      A200EECEA200F0D1A100B781820000000000078DBE0072D6F800078DBE00ACF7
      FC007BDCFA007BDCFA007BDCFA007BDCFA007BDCFA007BDCFA007BDCFA007BDC
      FA0048B9D900ACF7FC00078DBE00000000000000000000660000009800000098
      0000E5E1E5000066000000660000E5E1E500E7E6E700E5E1E500CDCCCD000066
      0000007D00000066000000000000000000000000000097433F00CD656600C162
      6200E5E3E500922B2B00922B2B00E4E0E400E6E5E700E4E0E400CECBCC00922B
      2B009E43410097433F0000000000000000000000000000000000CAAEA300FBF5
      EF00F8EAD900F7E7D500F5E4CF00F3DEC500F2D9B900F2D8B300F2D4AA00F0D1
      A100EECEA200F0D1A100B781820000000000078DBE007BDCFA001396B60099F0
      FC0092EBFB0086E3FB0086E3FB0086E3FB0086E3FB0086E3FB0086E3FB0086E3
      FB0048B9D900B1F7FC00078DBE00000000000000000000660000009800000098
      0000E5E4E6000066000000660000E4DCE500E7E6E700E5E4E600CDCCCD000066
      0000007D00000066000000000000000000000000000097433F00CD656600C162
      6200E5E3E500922B2B00922B2B00E4E0E400E6E5E700E6E5E700CECBCC00922B
      2B009E43410097433F0000000000000000000000000000000000CEACA800FDFA
      F600FAF1E800F8EAD900F7E7D500F3E1CC00F3DEC500F2DABD00F2D8B300F2D4
      AA00F0D1A100F0D1A100B781820000000000078DBE0086E3FB0048B9D90058C3
      E700ACF7FC008FE9FB008FE9FB008FE9FB008FE9FB008FE9FB008FE9FB000C84
      18004BBBDD00B6F7FD0066CDF200078DBE000000000000660000009800000098
      0000E7E6E700E7E6E700E5E1E500E4DCE500E5E1E500E5E1E500CDCCCD000066
      0000007D00000066000000000000000000000000000097433F00CD656600C162
      6200E6E5E700E6E5E700E4E0E400E4E0E400E4E0E400E4E0E400CECBCC00922B
      2B009E43410097433F0000000000000000000000000000000000D0A9AB00FEFC
      FB00FBF5EF00FAF1E800F8EAD900F7E7D500F4E0C800F3DEC500F2D9B900F2D8
      B300F2D4AA00F1D3A600B781820000000000078DBE008CE7FB0077DAF9001A9D
      AA00D8F7FB00CAF6FD00CAF6FD00CAF6FD00CAF6FD00CAF6FD000C84180035BC
      73000C841800D8F7FB00D6F6FB00078DBE000000000000660000009800000098
      0000009800000098000000980000009800000098000000980000009800000098
      0000009800000066000000000000000000000000000097433F00CD656600C663
      6300C8676700C6717000C6717000C86A6A00C4636300C86C6C00CA666600C463
      6300CD65660097433F0000000000000000000000000000000000E6BDAF00FEFC
      FB00FDFAF600FBF5EF00F9EDDE00F8EAD900F7E7D500F5E4CF00F3DEC500F2D9
      B900F2D8B300F2D6AF00B781820000000000078DBE0095EEFC0095EEFC001396
      B600078DBE00078DBE00078DBE00078DBE00078DBE000C84180046CC80004BCC
      98003DC374000C841800078DBE00078DBE00000000000066000000980000B4C6
      B000B4C6B000B4C6B000B4C6B000B4C6B000B4C6B000B4C6B000B4C6B000B4C6
      B000009800000066000000000000000000000000000097433F00B8646400C47C
      7B00C89E9E00CAA8A800CAA8A800CAA8A800C9A0A000C9A0A000CAA8A800CAA8
      A800CC66660097433F0000000000000000000000000000000000E6BDAF00FEFC
      FB00FEFCFB00FCF8F400FBF5EF00F9EDDE00F8EAD900F7E7D500F3E1CC00F2DC
      C100F2D9B900F2D8B300B781820000000000078DBE009EF4FD009FF6FD009FF6
      FD009EF4FD009FF6FD009FF6FD009EF4FD000C84180046CC800049CD890049CD
      89004BCC98003DC374000C84180000000000000000000066000000980000F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800009800000066000000000000000000000000000097433F00CC666600F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800CC66660097433F0000000000000000000000000000000000E6BDAF00FEFC
      FB00FEFCFB00FEFCFB00FCF8F400FBF5EF00F9EDDE00F8EAD900F7E7D500F4E0
      C800F4E0C800F2D9B900B781820000000000078DBE00D8F7FB00A2F7FD00A2F7
      FD00A2F7FD00A2F7FD00A2F7FD000C8418000C8418000C8418000C84180049CD
      890046CC80000C8418000C8418000C841800000000000066000000980000F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800009800000066000000000000000000000000000097433F00CC666600F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800CC66660097433F0000000000000000000000000000000000E7C4AD00FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00FCF8F400FBF5EF00F9EDDE00F9EDDE00F8EA
      D900F2DCC100CCAFA600B78182000000000000000000078DBE00D8F7FB00A5F7
      FC00A5F7FC00A5F7FC00078DBB0048B9D90048B9D90048B9D9000C84180046CC
      800035BC73000C8418000000000000000000000000000066000000980000F8F8
      F800CDCCCD00CDCCCD00CDCCCD00CDCCCD00CDCCCD00CDCCCD00CDCCCD00F8F8
      F800009800000066000000000000000000000000000097433F00CC666600F8F8
      F800CDCBCC00CDCBCC00CDCBCC00CDCBCC00CDCBCC00CDCBCC00CDCBCC00F8F8
      F800CC66660097433F0000000000000000000000000000000000E7C4AD00FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FCF8F400FBF5EF00F3E1CC00C6A1
      9600C0958800BA8F8600B7818200000000000000000000000000078DBE00078D
      BE00078DBE00078DBE00000000000000000000000000000000000C8418003FC6
      79000C841800000000000000000000000000000000000066000000980000F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800009800000066000000000000000000000000000097433F00CC666600F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800CC66660097433F0000000000000000000000000000000000EACAAC00FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFBF900F3DEC500C188
      7500DF9D5600DF9D5600C58B7200000000000000000000000000000000000000
      000000000000000000000000000000000000000000000C84180030B8720030B8
      72000C841800000000000000000000000000000000000066000000980000F8F8
      F800CDCCCD00CDCCCD00CDCCCD00CDCCCD00CDCCCD00CDCCCD00CDCCCD00F8F8
      F800009800000066000000000000000000000000000097433F00CC666600F8F8
      F800CDCBCC00CDCBCC00CDCBCC00CDCBCC00CDCBCC00CDCBCC00CDCBCC00F8F8
      F800CC66660097433F0000000000000000000000000000000000EACAAC00FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00F3E1CC00C39A
      8D00EECEA200CD906A0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000C84180030B872000C84
      180000000000000000000000000000000000000000000066000000980000F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800009800000066000000000000000000000000000097433F00CC666600F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800CC66660097433F0000000000000000000000000000000000EACAAC00FCF8
      F400FCF8F400FCF8F400FCF8F400FBF5EF00FBF5EF00FBF5EF00F3E1CC00C197
      8900C39A8D000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000C8418000C8418000C8418000C8418000000
      000000000000000000000000000000000000000000000000000000660000F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F80000660000000000000000000000000000000000000000000097433F00F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F80097433F000000000000000000000000000000000000000000E9C8AB00EACA
      AC00EACAAC00EACAAC00EACAAC00EACAAC00EACAAC00EACAAC00E6BDAF00BD85
      7900000000000000000000000000000000000000000000000000000000000000
      0000000000000C8418000C8418000C8418000C84180000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A5787300A5787300A5787300A5787300A5787300A5787300A578
      7300A5787300A57873008C5D5C00000000000000000000000000000000000000
      00000000000000000000A4410800A4410700A23F080000000000000000000000
      000000000000000000000000000000000000000000000000000000669A000066
      9A0000669A00A37F7700A37F7700A37F7700A37F7700A37F7700A37F7700A37F
      7700A37F7700A37F77008F626100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000632DE000632DE000000000000000000000000000000
      000000000000A97B7500FCE3CA00FDE1C500FDDFC100FCDAB900FCDAB900F9D4
      B000F9D4B000F5CCA6008C5D5C00000000000000000000000000000000000000
      000000000000A23F0800A4410800A23F0800A441080000000000000000000000
      0000000000000000000000000000000000000000000000669A004BC3E4004BC3
      E4004BC3E400B3847600F7ECDE00FAF1E700F8EEE100F7ECDE00F7ECDE00F7EC
      DE00F7ECDE00F7ECDE008F62610000000000000000000632DE000632DE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000632DE000632DE00000000000000000000000000000000000000
      000000000000AD7E7500FAE6D400E5A55600E5A55600E5A55600E5A55600E5A5
      5600E5A55600F9D4B0008C5D5C00000000000000000000000000000000000000
      000000000000A44107000000000000000000A23F0800A23F080000000000A23F
      0800A44107000000000000000000000000000000000000669A004AC5E6004BC3
      E4004BC3E400B3847600F5E7D800E3A55B00E4A55900E4A55900E4A55900E4A5
      5900E4A55900F5E3D1008F62610000000000000000000632DE000632DE000632
      DE00000000000000000000000000000000000000000000000000000000000000
      00000632DE000632DE0000000000000000000000000000000000000000000000
      000000000000B5867A00FBEEE200F9E9D900FBE4CF00FCE3CA00FDE1C500FCDD
      BD00FCDAB900F9D4B0008C5D5C00000000000000000000000000000000000000
      000000000000A44107000000000000000000A23F080000000000A23F0800A23F
      0800A23F0800A441080000000000000000000000000000669A004CC7E9004AC5
      E6004BC3E400B9877500F7ECDE00F5E4D200EFDCC900EFDCC900EFDCC900EBD8
      C600EFDCC900F5E4D2008F62610000000000000000000632DE000632DD000632
      DE000632DE000000000000000000000000000000000000000000000000000632
      DE000632DE00000000000000000000000000A5787300A5787300A5787300A578
      7300A5787300BA8C7D00FBF1E700E5A55600E5A55600E5A55600E5A55600E5A5
      5600E5A55600FCDAB9008C5D5C00000000000000000000000000000000000000
      000000000000A4410800A23F0800A23F0800A23F080000000000A23F08000000
      000000000000A441070000000000000000000000000000669A0057CDED0050C9
      EA004AC5E600B9877500F9EFE400E3A55B00E4A55900E4A55900E4A55900E4A5
      5900E4A55900F5E5D5008F6261000000000000000000000000000433ED000632
      DE000632DE000632DE00000000000000000000000000000000000632DE000632
      DE0000000000000000000000000000000000A97B7500FCE3CA00FDE1C500FDDF
      C100FCDAB900BD918400FDF5ED00FBF1E700FBEEE200F9E9D900FAE6D400FCE3
      CA00FDE1C500FDDFC1008C5D5C00000000000000000000000000000000000000
      00000000000000000000A23F0800A4410700A4410800A23F0800A44108000000
      000000000000A23F080000000000000000000000000000669A005DD1EF0057CD
      ED0053CBEB00C18C7200FAF2E900F7ECDE00F5E4D200F5E4D200F5E3D100F3E1
      CD00F5E4D200F5E9DB008F626100000000000000000000000000000000000000
      00000632DE000632DE000632DD00000000000632DD000632DE000632DE000000
      000000000000000000000000000000000000AD7E7500FAE6D400E5A55600E5A5
      5600E5A55600BD918400FEF9F300E5A55600E5A55600E5A55600E5A55600E5A5
      5600E5A55600FCE3CA008C5D5C00000000000000000000000000000000000000
      00000000000000000000000000009E420E0091471E00A23F0800A4410700A441
      0800A4410800A23F080000000000000000000000000000669A0065D6F20061D4
      F1005BD0EE00C6906F00FAF4EE00E3A55B00E4A55900E4A55900E4A55900E4A5
      5900E4A55900F9EEE2008F626100000000000000000000000000000000000000
      0000000000000632DD000533E7000533E7000533E9000632DD00000000000000
      000000000000000000000000000000000000B5867A00FBEEE200F9E9D900FBE4
      CF00FCE3CA00DEAB8400FEFBF900FEF9F500FDF7F000FCF4EA00FBF1E700FBEE
      E200F9E9D900FAE5D1008C5D5C00000000000000000000000000000000000000
      0000000000000000000000000000917E75008E7B71007C4E3500A23F0800A23F
      0800A23F08000000000000000000000000000000000000669A006DDBF50069D9
      F40061D4F100C6906F00FBF8F400FBF6F200F9F0E600F9EFE400F8EEE100F9EE
      E200F9F0E600F5E9DB008F626100000000000000000000000000000000000000
      000000000000000000000632E4000632E4000433EF0000000000000000000000
      000000000000000000000000000000000000BA8C7D00FBF1E700E5A55600E5A5
      5600E5A55600DEAB8400FEFBF900FEFBF900FEF9F500FEF9F300FDF5ED00F9E9
      D900ECC5A200BD9184008C5D5C00000000000000000000000000000000000000
      000000000000000000008E7C72009C918D008E7C72008E7C7200000000000000
      0000000000000000000000000000000000000000000000669A0075DFF80071DD
      F60069D9F400D5A58900FBF8F400FCFAF900FCFAF900FCFAF900FCFAF900B384
      7600B3847600B3847600B3847600000000000000000000000000000000000000
      0000000000000632DD000433ED000533E9000433EF000434F400000000000000
      000000000000000000000000000000000000BD918400FDF5ED00FBF1E700FBEE
      E200F9E9D900E2B18A00FEFBF900FEFBF900FEFBF900FEFBF800FEF9F300B281
      7600B2817600B2817600B07F7500000000000000000000000000000000000000
      0000000000008E7C7200C8BEBD008E7C72009D9591008E7C7200000000000000
      0000000000000000000000000000000000000000000000669A007CE2F90078E1
      F90071DDF600D5A58900FCFAF800FCFAF900FCFAF900FCFAF900FCFAF900B384
      7600DDA57200E2A45B0000000000000000000000000000000000000000000000
      00000434F4000433EF000533EB0000000000000000000434F4000335F8000000
      000000000000000000000000000000000000BD918400FEF9F300E5A55600E5A5
      5600E5A55600E5B68E00FEFBF900FEFBF900FEFBF900FEFBF900FEFBF800B281
      7600E5AE7000E4A3530000000000000000000000000000000000000000000000
      00008E7C7200D3CBCB008E7C72009D959100C6BBB9008E7C7200000000000000
      0000000000000000000000000000000000000000000000669A007EE3F9007CE2
      F90078E1F900D5A58900D9A68200D9A68200D9A68200D9A68200D9A68200B384
      7600C6AE9A0000669A0000000000000000000000000000000000000000000335
      F8000433EF000334F800000000000000000000000000000000000335F8000335
      F80000000000000000000000000000000000DEAB8400FEFBF900FEF9F500FDF7
      F000FCF4EA00E5B68E00DEAB8400DEAB8400DEAB8400DEAB8400DEAB8400B281
      7600E8AB5E000000000000000000000000000000000000000000000000008E7C
      7200D3CBCB008E7C7200000000008E7C7200D3CBCB008E7C7200000000000000
      0000000000000000000000000000000000000000000000669A0088E5F9007EE3
      F9007EE3F9007EE3F90078E1F90075DFF8006DDBF50065D6F2005DD1EF0057CD
      ED0053CBEB0000669A00000000000000000000000000000000000335F8000335
      F8000335F8000000000000000000000000000000000000000000000000000335
      F8000335F800000000000000000000000000DEAB8400FEFBF900FEFBF900FEF9
      F500FEF9F300FDF5ED00F9E9D900ECC5A200BD9184008C5D5C00000000000000
      0000000000000000000000000000000000000000000000000000000000008E7C
      72008E7C720000000000000000008E7C7200D3CBCB008E7C7200000000000000
      0000000000000000000000000000000000000000000000669A0088E5F90088E5
      F9007372720073727200737272007372720073727200737272007372720061D4
      F1005BD0EE0000669A000000000000000000000000000335F8000335F8000335
      F800000000000000000000000000000000000000000000000000000000000000
      0000000000000335F8000000000000000000E2B18A00FEFBF900FEFBF900FEFB
      F900FEFBF800FEF9F300B2817600B2817600B2817600B07F7500000000000000
      0000000000000000000000000000000000000000000000000000000000008E7C
      72000000000000000000000000008E7C7200D3CBCB008E7C7200000000000000
      0000000000000000000000000000000000000000000000669A0088E5F90088E5
      F90073727200D1C5BA00D1C5BA00D1C5BA00D1C5BA00C9BFB6007372720069D9
      F40061D4F10000669A0000000000000000000335F8000335F8000335F8000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E5B68E00FEFBF900FEFBF900FEFB
      F900FEFBF900FEFBF800B2817600E5AE7000E4A3530000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008E7C72008E7C720000000000000000000000
      000000000000000000000000000000000000000000000000000000669A000066
      9A0073727200EBD8C600FAFAF900FCF9F700FCF9F700D1C5BA00737272000066
      9A0000669A000000000000000000000000000335F8000335F800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E5B68E00DEAB8400DEAB8400DEAB
      8400DEAB8400DEAB8400B2817600E8AB5E000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008E7C72000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007372720073727200737272007372720073727200000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000800000000100010000000000000400000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFC7FFFFFF81FF707FC7F000FE007
      F007FC7F000FC003F707FC7FFFFF8001F7FFFC7FEE078001F707FC7FCE070000
      F007FC7F82070000F707FC7F03FF0000F7FFFC7F82000000F707FC7FCE000000
      F007FC7FEE000000F707F83FFFFF8001C1FF0000000F8001C1FF0000000FC003
      C1FF0000FFFFE007FFFFF83FFFFFF81FFFFFFFFDFFFF9FFFE003FFF0000F0FFF
      E003CFE1000F07FFE003E603FFFF83FFE003FC07DE07C1FFE003F807CE07E10F
      E003F0070607F003E003E00703FFF801E003E0070600F801E003E00FCE00F800
      E003E01FDE00F800E003E027FFFFF800E007C073000FF801E00F87FF000FFC01
      E01F0FFFFFFFFE03FFFF3FFFFFFFFF0FFFFFFFFF80038007E07FFE3F00010003
      C01FFC0F00010001C00FF80300010001C007F00000010001C007E00000010000
      C007C00000010000C007800000010000C007800000070007C007800100070007
      E00F8003800F800FF21F8007C3FFC3FFF39F800FFFFFFFFFF13F001FFFFFFFFF
      F83F063FFFFFFFFFFFFFCFFFFFFFFFFF9FFFFFFFFFFFE07F0FFFC003C003C01F
      07FF80018001C00F83FF80018001C007C1FF80018001C007E10F80018001C007
      F00380018001C007FC0180018001C007FC0180018001E007F80080018001FC07
      F80080018001FF9FF80080018001F39FFC0180018001F39FFC0180018001F39F
      FE03C003C003F83FFF8FFFFFFFFFFC7FF81FF81FF33FFE3FE007E007E007E007
      C003C003C000E007800180018001800380018001000080010000000000008001
      0000000000008001000000008000800100000000E001800100000000F003C003
      00000000F01FE00780018001E03FF81F80018001E03FF81FC003C003C03FF81F
      E007E007C07FF81FF81FF81FF8FFFC3FC0018003FFFFFFFFC0010003C007C007
      C001000180038003C001000180038003C001000180038003C001000080038003
      C001000080038003C001000080038003C001000180038003C001000080038003
      C001800380038003C001C3C780038003C001FF8780038003C003FF8F80038003
      C007FE1FC007C007C00FF87FFFFFFFFFF801FC7FC001FFFCF801F87F80019FF9
      F801FB2780018FF3F801FB43800187E70001F85B8001C3CF0001FC1B8001F11F
      0001FE038001F83F0001FE078001FC7F0001FC3F8001F83F0001F83F8003F19F
      0003F03F8003E3CF0007E23F8003C7E7003FE63F80038FFB003FEE3F80031FFF
      007FFE7FC0073FFF00FFFEFFF83FFFFF00000000000000000000000000000000
      000000000000}
  end
  object amMenu: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Items = <
              item
                Action = acNew
                Caption = '&New'
                ImageIndex = 30
                ShortCut = 16462
              end
              item
                Caption = '-'
              end
              item
                Action = acSave
                Caption = '&Save'
                ImageIndex = 21
                ShortCut = 16467
              end
              item
                Action = acSaveAs
                Caption = 'S&ave As...'
                ImageIndex = 22
              end
              item
                Caption = '-'
              end
              item
                Action = acOpen
                Caption = '&Open'
                ImageIndex = 16
                ShortCut = 16463
              end
              item
                Caption = '-'
              end
              item
                Action = acClose
                Caption = '&Close'
                ImageIndex = 3
                ShortCut = 16430
              end
              item
                Action = acCloseAllFiles
                Caption = 'C&lose all files'
                ImageIndex = 4
                ShortCut = 24622
              end
              item
                Caption = '-'
              end
              item
                Caption = '-'
              end
              item
                Action = acExit
                Caption = '&Exit'
                ImageIndex = 9
                ShortCut = 16465
              end>
            Caption = '&File'
          end
          item
            Items = <
              item
                Action = acUndo
                Caption = '&Undo'
                ImageIndex = 2
                ShortCut = 16474
              end
              item
                Action = acRedo
                Caption = '&Redo'
                ImageIndex = 13
                ShortCut = 24666
              end
              item
                Caption = '-'
              end
              item
                Action = acCut
                Caption = '&Cut'
                ImageIndex = 6
                ShortCut = 16472
              end
              item
                Action = acCopy
                Caption = 'C&opy'
                ImageIndex = 5
                ShortCut = 16451
              end
              item
                Action = acPaste
                Caption = '&Paste'
                ImageIndex = 18
                ShortCut = 16470
              end>
            Caption = '&Edit'
          end
          item
            Items = <
              item
                Action = acSearch
                Caption = '&Search'
                ImageIndex = 25
                ShortCut = 16454
              end
              item
                Action = acFindNext
                Caption = '&Find Next...'
                ImageIndex = 26
                ShortCut = 114
              end
              item
                Caption = '-'
              end
              item
                Action = acReplace
                Caption = '&Replace'
                ImageIndex = 28
                ShortCut = 16466
              end>
            Caption = '&Search'
          end
          item
            Items = <
              item
                Action = acCompile
                Caption = '&Compile'
                ImageIndex = 19
                ShortCut = 120
              end
              item
                Action = acCompileAndUpload
                Caption = 'C&ompile and upload'
                ShortCut = 8312
              end
              item
                Caption = '-'
              end>
            Caption = '&Compile'
          end
          item
            Items = <
              item
                Action = acDoc
                Caption = '&AMX Mod X Documentation'
                ImageIndex = 12
                ShortCut = 112
              end
              item
                Action = acForum
                ImageIndex = 12
                ShortCut = 8304
              end
              item
                Caption = '-'
              end
              item
                Action = acAbout
                Caption = 'A&bout...'
                ImageIndex = 14
              end>
            Caption = '&Help'
          end>
      end
      item
        Items = <
          item
            Action = acNew
            Caption = '&New'
            ImageIndex = 4
            ShowCaption = False
            ShortCut = 16462
          end
          item
            Action = acOpen
            Caption = '&Open'
            ImageIndex = 5
            ShowCaption = False
            ShortCut = 16463
          end
          item
            Action = acSave
            Caption = '&Save'
            ImageIndex = 6
            ShowCaption = False
            ShortCut = 16467
          end
          item
            Caption = '-'
          end
          item
            Action = acSearch
            Caption = 'S&earch'
            ImageIndex = 12
            ShowCaption = False
            ShortCut = 16454
          end
          item
            Caption = '-'
          end
          item
            Action = acCompile
            Caption = '&Compile'
            ImageIndex = 8
            ShowCaption = False
            ShortCut = 120
          end>
        ActionBar = atbToolBar
      end
      item
        Items = <
          item
            Items = <
              item
                Action = acNew
                Caption = '&New'
                ImageIndex = 4
                ShortCut = 16462
              end
              item
                Caption = '-'
              end
              item
                Action = acSave
                Caption = '&Save'
                ImageIndex = 6
                ShortCut = 16467
              end
              item
                Action = acSaveAs
                Caption = 'S&ave As...'
                ImageIndex = 7
              end
              item
                Action = acSaveAll
                Caption = 'Sa&ve All Files'
                ShortCut = 24659
              end
              item
                Caption = '-'
              end
              item
                Action = acOpen
                Caption = '&Open'
                ImageIndex = 5
                ShortCut = 16463
              end
              item
                Caption = '-'
              end
              item
                Action = acClose
                Caption = '&Close'
                ShortCut = 16430
              end
              item
                Action = acCloseAllFiles
                Caption = 'C&lose all files'
                ShortCut = 24622
              end
              item
                Caption = '-'
              end
              item
                Action = acExit
                Caption = '&Exit'
                ImageIndex = 3
                ShortCut = 16465
              end>
            Caption = '&File'
          end
          item
            Items = <
              item
                Caption = '-'
              end
              item
                Action = acUndo
                Caption = '&Undo'
                ImageIndex = 14
                ShortCut = 16474
              end
              item
                Action = acRedo
                Caption = '&Redo'
                ImageIndex = 13
                ShortCut = 24666
              end
              item
                Caption = '-'
              end
              item
                Action = acCut
                Caption = '&Cut'
                ImageIndex = 1
                ShortCut = 16472
              end
              item
                Action = acCopy
                Caption = 'C&opy'
                ImageIndex = 0
                ShortCut = 16451
              end
              item
                Action = acPaste
                Caption = '&Paste'
                ImageIndex = 2
                ShortCut = 16470
              end
              item
                Caption = '-'
              end
              item
                Action = acSelectAll
                Caption = '&Select all'
                ShortCut = 16449
              end>
            Caption = '&Edit'
          end
          item
            Items = <
              item
                Action = acSearch
                Caption = '&Search'
                ImageIndex = 12
                ShortCut = 16454
              end
              item
                Action = acFindNext
                Caption = '&Find Next...'
                ImageIndex = 23
                ShortCut = 114
              end
              item
                Caption = '-'
              end
              item
                Action = acReplace
                Caption = '&Replace'
                ImageIndex = 28
                ShortCut = 16466
              end
              item
                Caption = '-'
              end
              item
                Action = acGoTo
                Caption = '&Go to line...'
                ShortCut = 16455
              end>
            Caption = '&Search'
          end
          item
            Items = <
              item
                Action = acCompile
                Caption = '&Compile'
                ImageIndex = 8
                ShortCut = 120
              end
              item
                Caption = '-'
              end
              item
                Action = acCompileAndStart
                Caption = 'C&ompile and start Half-Life'
              end
              item
                Action = acCompileAndUpload
                Caption = 'Co&mpile and upload'
                ImageIndex = 21
                ShortCut = 8312
              end>
            Caption = '&Compile'
          end
          item
            Items = <
              item
                Action = acOptions
                Caption = '&Options'
                ImageIndex = 11
              end>
            Caption = 'Se&ttings'
          end
          item
            Items = <
              item
                Action = acIdenter
                Caption = '&Identer'
                ImageIndex = 22
                ShortCut = 16457
              end
              item
                Action = acUnidenter
                Caption = '&Unidenter'
                ImageIndex = 26
                ShortCut = 24649
              end
              item
                Caption = '-'
              end
              item
                Action = acLoopGenerator
                Caption = '&Loop Generator'
                ImageIndex = 27
              end
              item
                Caption = '-'
              end
              item
                Action = acMenuMaker
                Caption = '&Menu Maker'
                ImageIndex = 24
                ShortCut = 16461
              end
              item
                Action = acPMM
                Caption = '&Player Menu Maker'
                ImageIndex = 24
                ShortCut = 16464
              end
              item
                Caption = '-'
              end
              item
                Action = acSocketTerminal
                Caption = '&Socket Terminal'
                ImageIndex = 25
              end
              item
                Caption = '-'
              end
              item
                Action = acRemoveMissingPlugins
                Caption = '&Remove missing plugins from plugins.ini'
              end>
            Caption = 'T&ools'
          end
          item
            Items = <
              item
                Action = acDoc
                Caption = '&AMX Mod X Documentation'
                ShortCut = 112
              end
              item
                Action = acForum
                Caption = 'A&MX Mod X Scripting Forum'
                ShortCut = 8304
              end
              item
                Caption = '-'
              end
              item
                Action = acAbout
                Caption = 'A&bout...'
              end>
            Caption = '&Help'
          end>
        ActionBar = mmbMenu
      end>
    Images = ilMenu
    OnExecute = amMenuExecute
    Left = 826
    Top = 34
    StyleName = 'XP Style'
    object acNew: TAction
      Category = 'File'
      Caption = 'New'
      ImageIndex = 4
      ShortCut = 16462
      OnExecute = acNewExecute
    end
    object acSave: TAction
      Category = 'File'
      Caption = 'Save'
      ImageIndex = 6
      ShortCut = 16467
      OnExecute = acSaveExecute
    end
    object acSaveAs: TAction
      Category = 'File'
      Caption = 'Save As...'
      ImageIndex = 7
      OnExecute = acSaveAsExecute
    end
    object acSaveAll: TAction
      Category = 'File'
      Caption = 'Save All Files'
      ShortCut = 24659
      OnExecute = acSaveAllExecute
    end
    object acOpen: TAction
      Category = 'File'
      Caption = 'Open'
      ImageIndex = 5
      ShortCut = 16463
      OnExecute = acOpenExecute
    end
    object acClose: TAction
      Category = 'File'
      Caption = 'Close'
      Enabled = False
      ShortCut = 16430
      OnExecute = acCloseExecute
    end
    object acCloseAllFiles: TAction
      Category = 'File'
      Caption = 'Close all files'
      Enabled = False
      ShortCut = 24622
      OnExecute = acCloseAllFilesExecute
    end
    object acExit: TAction
      Category = 'File'
      Caption = 'Exit'
      ImageIndex = 3
      ShortCut = 16465
      OnExecute = acExitExecute
    end
    object acUndo: TAction
      Category = 'Edit'
      Caption = 'Undo'
      ImageIndex = 14
      ShortCut = 16474
      OnExecute = acUndoExecute
    end
    object acRedo: TAction
      Category = 'Edit'
      Caption = 'Redo'
      ImageIndex = 13
      ShortCut = 24666
      OnExecute = acRedoExecute
    end
    object acCut: TAction
      Category = 'Edit'
      Caption = 'Cut'
      ImageIndex = 1
      ShortCut = 16472
      OnExecute = acCutExecute
    end
    object acCopy: TAction
      Category = 'Edit'
      Caption = 'Copy'
      ImageIndex = 0
      ShortCut = 16451
      OnExecute = acCopyExecute
    end
    object acPaste: TAction
      Category = 'Edit'
      Caption = 'Paste'
      ImageIndex = 2
      ShortCut = 16470
      OnExecute = acPasteExecute
    end
    object acSelectAll: TAction
      Category = 'Edit'
      Caption = 'Select all'
      ShortCut = 16449
      OnExecute = acSelectAllExecute
    end
    object acSearch: TAction
      Category = 'Search'
      Caption = 'Search'
      ImageIndex = 12
      ShortCut = 16454
      OnExecute = acSearchExecute
    end
    object acFindNext: TAction
      Category = 'Search'
      Caption = 'Find Next...'
      ImageIndex = 23
      ShortCut = 114
      OnExecute = acFindNextExecute
    end
    object acReplace: TAction
      Category = 'Search'
      Caption = 'Replace'
      ImageIndex = 28
      ShortCut = 16466
      OnExecute = acReplaceExecute
    end
    object acCompile: TAction
      Category = 'Compile'
      Caption = 'Compile'
      ImageIndex = 8
      ShortCut = 120
      OnExecute = acCompileExecute
    end
    object acCompileAndStart: TAction
      Category = 'Compile'
      Caption = 'Compile and start CS'
      OnExecute = acCompileAndStartExecute
    end
    object acCompileAndUpload: TAction
      Category = 'Compile'
      Caption = 'Compile and upload'
      ImageIndex = 21
      ShortCut = 8312
      OnExecute = acCompileAndUploadExecute
    end
    object acDoc: TAction
      Category = 'Help'
      Caption = 'AMX Mod X Documentation'
      ShortCut = 112
      OnExecute = acDocExecute
    end
    object acForum: TAction
      Category = 'Help'
      Caption = 'AMX Mod X Scripting Forum'
      ShortCut = 8304
      OnExecute = acForumExecute
    end
    object acAbout: TAction
      Category = 'Help'
      Caption = 'About...'
      OnExecute = acAboutExecute
    end
    object acOptions: TAction
      Category = 'Settings'
      Caption = 'Options'
      ImageIndex = 11
      OnExecute = acOptionsExecute
    end
    object acGoTo: TAction
      Category = 'Search'
      Caption = 'Go to line...'
      ShortCut = 16455
      OnExecute = acGoToExecute
    end
    object acEdit: TAction
      Caption = 'Edit'
      ShortCut = 113
      OnExecute = acEditExecute
    end
    object acIdenter: TAction
      Category = 'Tools'
      Caption = 'Identer'
      ImageIndex = 22
      ShortCut = 16457
      OnExecute = acIdenterExecute
    end
    object acUnidenter: TAction
      Category = 'Tools'
      Caption = 'Unidenter'
      ImageIndex = 26
      ShortCut = 24649
      OnExecute = acUnidenterExecute
    end
    object acLoopGenerator: TAction
      Category = 'Tools'
      Caption = 'Loop Generator'
      ImageIndex = 27
      OnExecute = acLoopGeneratorExecute
    end
    object acMenuMaker: TAction
      Category = 'Tools'
      Caption = 'Menu Maker'
      ImageIndex = 24
      ShortCut = 16461
      OnExecute = acMenuMakerExecute
    end
    object acPMM: TAction
      Category = 'Tools'
      Caption = 'Player Menu Maker'
      ImageIndex = 24
      ShortCut = 16464
      OnExecute = acPMMExecute
    end
    object acSocketTerminal: TAction
      Category = 'Tools'
      Caption = 'Socket Terminal'
      ImageIndex = 25
      OnExecute = acSocketTerminalExecute
    end
    object acRemoveMissingPlugins: TAction
      Category = 'Tools'
      Caption = 'Remove missing plugins from plugins.ini'
      OnExecute = acRemoveMissingPluginsExecute
    end
  end
  object sacComplete: TSciAutoComplete
    NumStartChars = 1
    AStrings.Strings = (
      ''
      'access'
      'add'
      'ADMIN_ADMIN'
      'ADMIN_ALL'
      'ADMIN_BAN'
      'ADMIN_CFG'
      'ADMIN_CHAT'
      'ADMIN_CVAR'
      'ADMIN_IMMUNITY'
      'ADMIN_KICK'
      'ADMIN_LEVEL_A'
      'ADMIN_LEVEL_B'
      'ADMIN_LEVEL_C'
      'ADMIN_LEVEL_D'
      'ADMIN_LEVEL_E'
      'ADMIN_LEVEL_F'
      'ADMIN_LEVEL_G'
      'ADMIN_LEVEL_H'
      'ADMIN_MAP'
      'ADMIN_MENU'
      'ADMIN_PASSWORD'
      'ADMIN_RCON'
      'ADMIN_RESERVATION'
      'ADMIN_SLAY'
      'ADMIN_USER'
      'ADMIN_VOTE'
      'ALLIES'
      'AMX_FLAG_BIGENDIAN'
      'AMX_FLAG_BROWSE'
      'AMX_FLAG_COMPACT'
      'AMX_FLAG_DEBUG'
      'AMX_FLAG_LINEOPS'
      'AMX_FLAG_NOCHECKS'
      'AMX_FLAG_RELOC'
      'AMXX_VERSION'
      'AMXX_VERSION_STR[]="1'
      'anglevector'
      'assert'
      'attach_view'
      'ATTN_IDLE'
      'ATTN_NONE'
      'ATTN_STATIC'
      'AXIS'
      'BLOCK_NOT'
      'BLOCK_ONCE'
      'BLOCK_SET'
      'bomb_defused'
      'bomb_defusing'
      'bomb_explode'
      'bomb_planted'
      'bomb_planting'
      'break'
      'build_path'
      'call_think'
      'callfunc_begin'
      'callfunc_end'
      'callfunc_push_float'
      'callfunc_push_floatrf'
      'callfunc_push_int'
      'callfunc_push_intrf'
      'callfunc_push_str'
      'CAMERA_3RDPERSON'
      'CAMERA_NONE'
      'CAMERA_TOPDOWN'
      'CAMERA_UPLEFT'
      'case'
      'CHAN_AUTO'
      'CHAN_ITEM'
      'CHAN_NETWORKVOICE_BASE'
      'CHAN_NETWORKVOICE_END'
      'CHAN_STATIC'
      'CHAN_STREAM'
      'CHAN_WEAPON'
      'change_task'
      'char'
      'clamp'
      'client_authorized'
      'client_built'
      'client_changeclass'
      'client_changeteam'
      'client_cmd'
      'client_command'
      'client_connect'
      'client_damage'
      'client_death'
      'client_disconnect'
      'client_impulse'
      'client_infochanged'
      'client_kill'
      'client_PostThink'
      'client_PreThink'
      'client_print'
      'client_putinserver'
      'client_score'
      'client_spawn'
      'cmd_access'
      'cmd_target'
      'colored_menus'
      'console_cmd'
      'console_print'
      'const'
      'contain'
      'containi'
      'CONTENTS_TRANSLUCENT'
      'continue'
      'copy'
      'copy_keyvalue'
      'copyc'
      'create_entity'
      'CreateEntity'
      'cs_get_hostage_foll'
      'cs_get_hostage_id'
      'cs_get_no_knives'
      'cs_get_user_bpammo'
      'cs_get_user_buyzone'
      'cs_get_user_deaths'
      'cs_get_user_defuse'
      'cs_get_user_driving'
      'cs_get_user_hasprim'
      'cs_get_user_model'
      'cs_get_user_money'
      'cs_get_user_nvg'
      'cs_get_user_plant'
      'cs_get_user_stationary'
      'cs_get_user_team'
      'cs_get_user_tked'
      'cs_get_user_vip'
      'cs_get_weapon_ammo'
      'cs_get_weapon_burst'
      'cs_get_weapon_id'
      'cs_get_weapon_silen'
      'cs_reset_user_model'
      'cs_set_hostage_foll'
      'cs_set_no_knives'
      'cs_set_user_bpammo'
      'cs_set_user_deaths'
      'cs_set_user_defuse'
      'cs_set_user_model'
      'cs_set_user_money'
      'cs_set_user_nvg'
      'cs_set_user_plant'
      'cs_set_user_team'
      'cs_set_user_tked'
      'cs_set_user_vip'
      'cs_set_weapon_ammo'
      'cs_set_weapon_burst'
      'cs_set_weapon_silen'
      'cstrike_running'
      'CSW_AK47'
      'CSW_AUG'
      'CSW_AWP'
      'CSW_C4'
      'CSW_DEAGLE'
      'CSW_ELITE'
      'CSW_FAMAS'
      'CSW_FIVESEVEN'
      'CSW_FLASHBANG'
      'CSW_G3SG1'
      'CSW_GALI'
      'CSW_GALIL'
      'CSW_GLOCK18'
      'CSW_HEGRENADE'
      'CSW_KNIFE'
      'CSW_M249'
      'CSW_M3'
      'CSW_M4A1'
      'CSW_MAC10'
      'CSW_MP5NAVY'
      'CSW_P228'
      'CSW_P90'
      'CSW_SCOUT'
      'CSW_SG550'
      'CSW_SG552'
      'CSW_SMOKEGRENADE'
      'CSW_TMP'
      'CSW_UMP45'
      'CSW_USP'
      'CSW_XM1014'
      'current_num_ents'
      'custom_weapon_add'
      'custom_weapon_dmg'
      'custom_weapon_shot'
      'cvar_exists'
      'date'
      'dbi_close'
      'dbi_connect'
      'dbi_error'
      'dbi_field'
      'dbi_free_result'
      'dbi_nextrow'
      'dbi_num_rows'
      'dbi_query'
      'dbi_result'
      'dbi_type'
      'default'
      'define'
      'defined'
      'delete_file'
      'DF_Blocked'
      'DF_ClientCommand'
      'DF_ClientConnect'
      'DF_ClientDisconnect'
      'DF_ClientKill'
      'DF_ClientPutInServer'
      'DF_ClientUserInfoChanged'
      'DF_CreateInstancedBaseline'
      'DF_GameInit'
      'DF_GetGameDescription'
      'DF_GetHullBounds'
      'DF_MetaFunc_CallGameEntity'
      'DF_ParmsChangeLevel'
      'DF_ParmsNewLevel'
      'DF_pfnAllowLagCompensation'
      'DF_PlayerPostThink'
      'DF_PlayerPreThink'
      'DF_PM_FindTextureType'
      'DF_RegisterEncoders'
      'DF_ServerDeactivate'
      'DF_SetAbsBox'
      'DF_Spawn'
      'DF_SpectatorConnect'
      'DF_SpectatorDisconnect'
      'DF_SpectatorThink'
      'DF_StartFrame'
      'DF_Sys_Error'
      'DF_Think'
      'DF_Touch'
      'DF_Use'
      'DispatchKeyValue'
      'DispatchSpawn'
      'dllfunc'
      'DMG_ACID'
      'DMG_ALWAYSGIB'
      'DMG_BLAST'
      'DMG_BULLET'
      'DMG_BURN'
      'DMG_CLUB'
      'DMG_CRUSH'
      'DMG_DROWN'
      'DMG_DROWNRECOVER'
      'DMG_ENERGYBEAM'
      'DMG_FALL'
      'DMG_FREEZE'
      'DMG_GENERIC'
      'DMG_MORTAR'
      'DMG_NERVEGAS'
      'DMG_NEVERGIB'
      'DMG_PARALYZE'
      'DMG_POISON'
      'DMG_RADIATION'
      'DMG_SHOCK'
      'DMG_SLASH'
      'DMG_SLOWBURN'
      'DMG_SLOWFREEZE'
      'DMG_SONIC'
      'DMG_TIMEBASED'
      'do'
      'dod_get_map_info'
      'dod_get_next_class'
      'dod_get_pl_deaths'
      'dod_get_pl_teamname'
      'dod_get_pronestate'
      'dod_get_team_score'
      'dod_get_user_ammo'
      'dod_get_user_class'
      'dod_get_user_kills'
      'dod_get_user_score'
      'dod_get_user_weapon'
      'dod_is_deployed'
      'dod_is_randomclass'
      'dod_make_deathmsg'
      'dod_set_fuse'
      'dod_set_pl_deaths'
      'dod_set_pl_teamname'
      'dod_set_stamina'
      'dod_set_user_ammo'
      'dod_set_user_class'
      'dod_set_user_kills'
      'dod_set_user_score'
      'dod_set_user_team'
      'dod_user_kill'
      'dod_wpnlog_to_id'
      'dod_wpnlog_to_name'
      'DODMAX_WEAPONS'
      'drop_to_floor'
      'EF_AllocString'
      'EF_AngleVectors'
      'EF_AnimationAutomove'
      'EF_BuildSoundMSG'
      'EF_CanSkipPlayer'
      'EF_ChangeLevel'
      'EF_ChangePitch'
      'EF_ChangeYaw'
      'EF_CheckVisibility'
      'EF_CreateEntity'
      'EF_CreateFakeClient'
      'EF_CreateNamedEntity'
      'EF_CrosshairAngle'
      'EF_DecalIndex'
      'EF_DropToFloor'
      'EF_EmitAmbientSound'
      'EF_EmitSound'
      'EF_EntIsOnFloor'
      'EF_EntitiesInPVS'
      'EF_FadeClientVolume'
      'EF_FindClientInPVS'
      'EF_FindEntityByString'
      'EF_FindEntityInSphere'
      'EF_FreeEntPrivateData'
      'EF_GetAimVector'
      'EF_GetAttachment'
      'EF_GetBonePosition'
      'EF_GetClientListening'
      'EF_GetCurrentPlayer'
      'EF_GetEntityIllum'
      'EF_GetPhysicsInfoString'
      'EF_GetPhysicsKeyValue'
      'EF_InfoKeyValue'
      'EF_INVLIGHT'
      'EF_LIGHT'
      'EF_LightStyle'
      'EF_MakeStatic'
      'EF_MakeVectors'
      'EF_MessageBegin'
      'EF_ModelFrames'
      'EF_ModelIndex'
      'EF_MoveToOrigin'
      'EF_NODRAW'
      'EF_NOINTERP'
      'EF_NumberOfEntities'
      'EF_ParticleEffect'
      'EF_PlaybackEvent'
      'EF_PointContents'
      'EF_PrecacheEvent'
      'EF_PrecacheGeneric'
      'EF_PrecacheModel'
      'EF_PrecacheSound'
      'EF_RegUserMsg'
      'EF_RemoveEntity'
      'EF_RunPlayerMove'
      'EF_SetClientKeyValue'
      'EF_SetClientListening'
      'EF_SetClientMaxspeed'
      'EF_SetGroupMask'
      'EF_SetKeyValue'
      'EF_SetModel'
      'EF_SetOrigin'
      'EF_SetPhysicsKeyValue'
      'EF_SetSize'
      'EF_SetView'
      'EF_StaticDecal'
      'EF_SzFromIndex'
      'EF_Time'
      'EF_TraceHull'
      'EF_TraceLine'
      'EF_TraceModel'
      'EF_TraceMonsterHull'
      'EF_TraceSphere'
      'EF_TraceTexture'
      'EF_TraceToss'
      'EF_VecToAngles'
      'EF_VecToYaw'
      'EF_WalkMove'
      'EF_WriteAngle'
      'EF_WriteCoord'
      'else'
      'emit_sound'
      'endif'
      'engclient_cmd'
      'engclient_print'
      'engfunc'
      'ENT_SetModel'
      'ENT_SetOrigin'
      'entity_count'
      'entity_get_byte'
      'entity_get_edict'
      'entity_get_float'
      'entity_get_int'
      'entity_get_string'
      'entity_get_vector'
      'entity_range'
      'entity_set_byte'
      'entity_set_edict'
      'entity_set_float'
      'entity_set_int'
      'entity_set_model'
      'entity_set_origin'
      'entity_set_size'
      'entity_set_string'
      'entity_set_vector'
      'Entvars_Get_Byte'
      'Entvars_Get_Edict'
      'Entvars_Get_Float'
      'Entvars_Get_Int'
      'Entvars_Get_String'
      'Entvars_Get_Vector'
      'Entvars_Set_Byte'
      'Entvars_Set_Edict'
      'Entvars_Set_Float'
      'Entvars_Set_Int'
      'Entvars_Set_String'
      'Entvars_Set_Vector'
      'enum'
      'equal'
      'equali'
      'exit'
      'fake_touch'
      'fakedamage'
      'FakeTouch'
      'fclose'
      'FCVAR_CLIENTDLL'
      'FCVAR_EXTDLL'
      'FCVAR_PRINTABLEONLY'
      'FCVAR_PROTECTED'
      'FCVAR_SPONLY'
      'FCVAR_UNLOGGED'
      'feof'
      'fflush'
      'fgetc'
      'fgetf'
      'fgeti'
      'fgetl'
      'fgets'
      'file_exists'
      'file_size'
      'filesize'
      'find_ent'
      'find_ent_by_class'
      'find_ent_by_model'
      'find_ent_by_owner'
      'find_ent_by_target'
      'find_ent_by_tname'
      'find_ent_in_sphere'
      'find_ent_sphere'
      'find_entity'
      'find_player'
      'find_plugin_bydesc'
      'find_plugin_byfile'
      'find_sphere_class'
      'FindEntity'
      'FL_ALWAYSTHINK'
      'FL_BASEVELOCITY'
      'FL_CUSTOMENTITY'
      'FL_DORMANT'
      'FL_DUCKING'
      'FL_FAKECLIENT'
      'FL_FLOAT'
      'FL_FROZEN'
      'FL_GRAPHED'
      'FL_IMMUNE_LAVA'
      'FL_IMMUNE_WATER'
      'FL_KILLME'
      'FL_MONSTERCLIP'
      'FL_ONTRAIN'
      'FL_PROXY'
      'FL_SPECTATOR'
      'FL_WORLDBRUSH'
      'FLAG_AUTHID'
      'FLAG_IP'
      'FLAG_KICK'
      'FLAG_NOPASS'
      'FLAG_TAG'
      'float'
      'floatabs'
      'floatacos'
      'floatadd'
      'floatasin'
      'floatatan'
      'floatatan2'
      'floatcmp'
      'floatcos'
      'floatdiv'
      'floatfract'
      'floatlog'
      'floatmul'
      'floatpower'
      'floatround'
      'floatsin'
      'floatsqroot'
      'floatstr'
      'floatsub'
      'floattan'
      'FMRES_HANDLED'
      'FMRES_IGNORED'
      'FMRES_OVERRIDE'
      'FMRES_SUPERCEDE'
      'FMV_CELL'
      'FMV_FLOAT'
      'fopen'
      'for'
      'force_unmodified'
      'force_use'
      'format'
      'format_args'
      'format_time'
      'forward'
      'forward_return'
      'fputc'
      'fputf'
      'fputi'
      'fputl'
      'fputs'
      'fread'
      'fscanf'
      'fseek'
      'FT_NEW'
      'FT_OLD'
      'ftell'
      'funcidx'
      'FUSE_RESET'
      'FUSE_SET'
      'FVecIVec'
      'fwrite'
      'geoip_code2'
      'geoip_code3'
      'geoip_country'
      'get_basedir'
      'get_brush_entity_origin'
      'get_build'
      'get_class'
      'get_clcmd'
      'get_clcmdsnum'
      'get_client_listen'
      'get_concmd'
      'get_concmdsnum'
      'get_configsdir'
      'get_customdir'
      'get_cvar_flags'
      'get_cvar_float'
      'get_cvar_num'
      'get_cvar_string'
      'get_datadir'
      'get_decal_index'
      'get_distance'
      'get_entity_distance'
      'get_entity_flags'
      'get_entity_origin'
      'get_entity_velocity'
      'get_entity_visibility'
      'get_filename'
      'get_flags'
      'get_gametime'
      'get_global_edict'
      'get_global_float'
      'get_global_int'
      'get_global_string'
      'get_global_vector'
      'get_grenade'
      'get_grenade_id'
      'get_grenade_index'
      'get_hostage_id'
      'get_info_keybuffer'
      'get_keyvalue'
      'get_lang'
      'get_langsnum'
      'get_localinfo'
      'get_logfile'
      'get_mapname'
      'get_mask'
      'get_max_entities'
      'get_maxplayers'
      'get_maxspeed'
      'get_modname'
      'get_module'
      'get_modulesnum'
      'get_msg_arg_float'
      'get_msg_arg_int'
      'get_msg_arg_string'
      'get_msg_args'
      'get_msg_argtype'
      'get_msg_block'
      'get_msg_origin'
      'get_owner'
      'get_pdata'
      'get_pdata_char'
      'get_pdata_float'
      'get_pdata_int'
      'get_pdata_short'
      'get_players'
      'get_playersnum'
      'get_plugin'
      'get_pluginsnum'
      'get_private_f'
      'get_private_i'
      'get_range'
      'get_res'
      'get_spawn'
      'get_speak'
      'get_special'
      'get_speed'
      'get_speedchange'
      'get_srvcmd'
      'get_srvcmdsnum'
      'get_stats'
      'get_stats2'
      'get_statsnum'
      'get_string'
      'get_systime'
      'get_time'
      'get_timeleft'
      'get_tr'
      'get_user_aiming'
      'get_user_ammo'
      'get_user_armor'
      'get_user_astats'
      'get_user_attacker'
      'get_user_authid'
      'get_user_button'
      'get_user_deaths'
      'get_user_flags'
      'get_user_frags'
      'get_user_godmode'
      'get_user_gravity'
      'get_user_health'
      'get_user_hitzones'
      'get_user_index'
      'get_user_info'
      'get_user_ip'
      'get_user_lstats'
      'get_user_maxspeed'
      'get_user_menu'
      'get_user_money'
      'get_user_msgid'
      'get_user_msgname'
      'get_user_name'
      'get_user_noclip'
      'get_user_oldbutton'
      'get_user_origin'
      'get_user_ping'
      'get_user_rstats'
      'get_user_stats'
      'get_user_stats2'
      'get_user_team'
      'get_user_time'
      'get_user_userid'
      'get_user_velocity'
      'get_user_vstats'
      'get_user_weapon'
      'get_user_weapons'
      'get_user_wlstats'
      'get_user_wrstats'
      'get_user_wstats'
      'get_usercmd'
      'get_vaultdata'
      'get_weaponname'
      'get_xvar_float'
      'get_xvar_id'
      'get_xvar_num'
      'getarg'
      'getkey_float'
      'getkey_int'
      'getkey_string'
      'GetMessageBlock'
      'give_item'
      'globals_get_edict'
      'globals_get_float'
      'globals_get_int'
      'globals_get_string'
      'globals_get_vector'
      'goto'
      'gpglobals_v'
      'gpgobals_time'
      'grenade_throw'
      'halflife_time'
      'has_weapon'
      'heapspace'
      'HIT_CHEST'
      'HIT_GENERIC'
      'HIT_HEAD'
      'HIT_LEFTARM'
      'HIT_LEFTLEG'
      'HIT_RIGHTARM'
      'HIT_RIGHTLEG'
      'HIT_STOMACH'
      'HIW_AK47'
      'HIW_AKS74U'
      'HIW_BERETTA'
      'HIW_FLASHBANG'
      'HIW_GLOCK'
      'HIW_M11'
      'HIW_M11SD'
      'HIW_M16A2'
      'HIW_M4A1'
      'HIW_MP5A4'
      'HIW_MP5SD5'
      'HIW_NATOGREN'
      'HIW_PSG1'
      'HIW_REMINGTON'
      'HIW_SPAS12'
      'HIW_TANGOGREN'
      'HIW_ZASTAVA'
      'HLTime'
      'HULL_HEAD'
      'HULL_HUMAN'
      'HULL_LARGE'
      'HULL_POINT'
      'if'
      'IN_ALT1'
      'IN_ATTACK'
      'IN_ATTACK2'
      'IN_BACK'
      'IN_CANCEL'
      'IN_DUCK'
      'IN_FORWARD'
      'IN_JUMP'
      'IN_LEFT'
      'in_list_float'
      'in_list_int'
      'in_list_string'
      'IN_MOVELEFT'
      'IN_MOVERIGHT'
      'IN_RELOAD'
      'IN_RIGHT'
      'IN_RUN'
      'IN_SCORE'
      'IN_USE'
      'include'
      'inconsistent_file'
      'is_combat'
      'is_dedicated_server'
      'is_ent_valid'
      'is_entity'
      'is_jit_enabled'
      'is_linux_server'
      'is_map_valid'
      'is_module_loaded'
      'is_plugin_loaded'
      'is_running'
      'is_user_admin'
      'is_user_alive'
      'is_user_bot'
      'is_user_connected'
      'is_user_connecting'
      'is_user_hltv'
      'is_valid_ent'
      'isalnum'
      'isalpha'
      'isdigit'
      'isspace'
      'IVecFVec'
      'jghg_find_ent_owner'
      'jghg2_set_size'
      'jghg2_think'
      'keytable_clear'
      'keytable_count'
      'keytable_delete'
      'keytable_getkey'
      'keytable_getval'
      'keytable_next'
      'keytable_reset'
      'lang_exists'
      'LANG_PLAYER'
      'LANG_SERVER'
      'list_clear'
      'list_clear_float'
      'list_clear_int'
      'list_clear_string'
      'list_delete'
      'list_delete_float'
      'list_delete_int'
      'list_delete_string'
      'list_get'
      'list_get_float'
      'list_get_int'
      'list_get_string'
      'list_getf'
      'list_next'
      'list_next_float'
      'list_next_int'
      'list_next_string'
      'list_pop'
      'list_pop_float'
      'list_pop_int'
      'list_pop_string'
      'list_push_float'
      'list_push_int'
      'list_push_string'
      'list_reset'
      'list_reset_float'
      'list_reset_int'
      'list_reset_string'
      'list_size'
      'list_size_float'
      'list_size_int'
      'list_size_string'
      'list_store_float'
      'list_store_int'
      'list_store_string'
      'log_amx'
      'log_message'
      'log_to_file'
      'make_deathmsg'
      'make_string'
      'max'
      'md5'
      'md5_file'
      'MENU_KEY_0'
      'MENU_KEY_1'
      'MENU_KEY_2'
      'MENU_KEY_3'
      'MENU_KEY_4'
      'MENU_KEY_5'
      'MENU_KEY_6'
      'MENU_KEY_7'
      'MENU_KEY_8'
      'MENU_KEY_9'
      'message_begin'
      'message_end'
      'MessageBlock'
      'min'
      'MOVETYPE_ANGLECLIP'
      'MOVETYPE_ANGLENOCLIP'
      'MOVETYPE_BOUNCEMISSILE'
      'MOVETYPE_FOLLOW'
      'msg_args'
      'msg_data'
      'msg_data_type'
      'msg_dest'
      'msg_loc'
      'msg_name'
      'MSG_ONE_UNRELIABLE'
      'MSG_PAS'
      'MSG_PAS_R'
      'MSG_PVS'
      'MSG_PVS_R'
      'msg_set_f'
      'msg_set_i'
      'msg_set_s'
      'msg_strdata'
      'msg_type'
      'mysql_close'
      'mysql_connect'
      'mysql_error'
      'mysql_getfield'
      'mysql_nextrow'
      'mysql_query'
      'native'
      'new'
      'new_float_list'
      'new_int_list'
      'new_keytable'
      'new_list'
      'new_string_list'
      'NS_CONST_INC'
      'ns_get_build'
      'ns_get_class'
      'ns_get_deaths'
      'ns_get_energy'
      'ns_get_exp'
      'ns_get_hive_trait'
      'ns_get_jpfuel'
      'ns_get_mask'
      'ns_get_maxspeed'
      'ns_get_points'
      'ns_get_res'
      'ns_get_score'
      'ns_get_spawn'
      'ns_get_speedchange'
      'ns_get_struct_owner'
      'ns_get_weap_clip'
      'ns_get_weap_dmg'
      'ns_get_weap_range'
      'ns_get_weap_reserve'
      'ns_give_item'
      'ns_has_weapon'
      'NS_INC'
      'ns_is_combat'
      'ns_popup'
      'ns_set_deaths'
      'ns_set_energy'
      'ns_set_exp'
      'ns_set_fov'
      'ns_set_hive_trait'
      'ns_set_jpfuel'
      'ns_set_mask'
      'ns_set_player_body'
      'ns_set_player_model'
      'ns_set_player_skin'
      'ns_set_points'
      'ns_set_res'
      'ns_set_score'
      'ns_set_speedchange'
      'ns_set_struct_owner'
      'ns_set_weap_clip'
      'ns_set_weap_dmg'
      'ns_set_weap_range'
      'ns_set_weap_reserve'
      'ns2amx_getammo'
      'ns2amx_getenergy'
      'ns2amx_gethives'
      'ns2amx_getjpfuel'
      'ns2amx_giveitem'
      'ns2amx_inrange'
      'ns2amx_isdigesting'
      'ns2amx_moveto'
      'ns2amx_nspopup'
      'ns2amx_setammo'
      'ns2amx_setenergy'
      'ns2amx_setjpfuel'
      'ns2amx_setres'
      'ns2amx_version'
      'num_to_str'
      'num_to_word'
      'numargs'
      'number_of_entities'
      'numtostr'
      'operator'
      'parse'
      'parse_loguser'
      'parse_time'
      'pause'
      'pev'
      'pev_f'
      'pev_i'
      'pfn_keyvalue'
      'pfn_playbackevent'
      'pfn_spawn'
      'pfn_think'
      'pfn_touch'
      'PITCH_HIGH'
      'PITCH_LOW'
      'playback_event'
      'plugin_cfg'
      'PLUGIN_CONTINUE'
      'plugin_end'
      'plugin_flags'
      'PLUGIN_HANDLED'
      'PLUGIN_HANDLED_MAIN'
      'plugin_init'
      'plugin_log'
      'plugin_modules'
      'plugin_pause'
      'plugin_precache'
      'plugin_unpause'
      'point_contents'
      'PointContents'
      'power'
      'precache_event'
      'precache_generic'
      'precache_model'
      'precache_sound'
      'public'
      'radius_damage'
      'RadiusDamage'
      'random'
      'random_float'
      'random_num'
      'read_argc'
      'read_args'
      'read_argv'
      'read_data'
      'read_datanum'
      'read_dir'
      'read_file'
      'read_flags'
      'read_logargc'
      'read_logargv'
      'read_logdata'
      'regex_free'
      'regex_match'
      'regex_substr'
      'register_changelvl'
      'register_clcmd'
      'register_clientkill'
      'register_concmd'
      'register_cvar'
      'register_dictionary'
      'register_event'
      'register_forward'
      'register_impulse'
      'register_logevent'
      'register_menu'
      'register_menucmd'
      'register_menuid'
      'register_message'
      'register_msgblock'
      'register_msgedit'
      'register_playback'
      'register_plugin'
      'register_srvcmd'
      'register_statsfwd'
      'register_think'
      'register_touch'
      'remove_cvar_flags'
      'remove_entity'
      'remove_entity_name'
      'remove_quotes'
      'remove_task'
      'remove_user_flags'
      'remove_vaultdata'
      'RemoveEntity'
      'replace'
      'require_module'
      'reset_user_wstats'
      'return'
      'rewind'
      'SEEK_CUR'
      'SEEK_END'
      'SEEK_SET'
      'server_changelevel'
      'server_cmd'
      'server_exec'
      'server_frame'
      'server_print'
      'ServerFrame'
      'set_client_listen'
      'set_cvar_flags'
      'set_cvar_float'
      'set_cvar_num'
      'set_cvar_string'
      'set_entity_flags'
      'set_entity_origin'
      'set_entity_velocity'
      'set_entity_visibility'
      'set_hudmessage'
      'set_kvhandled'
      'set_lights'
      'set_localinfo'
      'set_mask'
      'set_msg_arg_float'
      'set_msg_arg_int'
      'set_msg_arg_string'
      'set_msg_block'
      'set_pdata'
      'set_pdata_char'
      'set_pdata_float'
      'set_pdata_int'
      'set_pdata_short'
      'set_pev'
      'set_pev_f'
      'set_pev_i'
      'set_player_body'
      'set_player_model'
      'set_player_skin'
      'set_private_f'
      'set_private_i'
      'set_rendering'
      'set_size'
      'set_speak'
      'set_speedchange'
      'set_task'
      'set_tr'
      'set_user_armor'
      'set_user_deaths'
      'set_user_flags'
      'set_user_footsteps'
      'set_user_frags'
      'set_user_godmode'
      'set_user_gravity'
      'set_user_health'
      'set_user_hitzones'
      'set_user_info'
      'set_user_maxspeed'
      'set_user_money'
      'set_user_noclip'
      'set_user_origin'
      'set_user_rendering'
      'set_user_velocity'
      'set_usercmd'
      'set_vaultdata'
      'set_view'
      'set_xvar_float'
      'set_xvar_num'
      'setarg'
      'setc'
      'SetSpeak'
      'SetView'
      'show_activity'
      'show_hudmessage'
      'show_menu'
      'show_motd'
      'sizeof'
      'sleep'
      'socket_change'
      'socket_close'
      'socket_open'
      'socket_recv'
      'socket_send'
      'SOCKET_TCP'
      'SOCKET_UDP'
      'spawn'
      'SPEAK_ALL'
      'SPEAK_LISTENALL'
      'SPEAK_MUTED'
      'SPEAK_NORMAL'
      'sqroot'
      'STAMINA_RESET'
      'STAMINA_SET'
      'static'
      'stock'
      'store_float'
      'store_int'
      'store_string'
      'str_to_num'
      'strbreak'
      'string'
      'strip_user_weapons'
      'strlen'
      'strpack'
      'strtok'
      'strtolower'
      'strtonum'
      'strtoupper'
      'strunpack'
      'supercede'
      'SVC_ADDANGLE'
      'SVC_CDTRACK'
      'SVC_INTERMISSION'
      'SVC_NEWUSERMSG'
      'SVC_ROOMTYPE'
      'SVC_TEMPENTITY'
      'SVC_WEAPONANIM'
      'swapchars'
      'switch'
      'take_damage'
      'task_exists'
      'tfc_clearmodel'
      'tfc_getbammo'
      'tfc_getweaponbammo'
      'tfc_isgrenade'
      'tfc_setbammo'
      'tfc_setmodel'
      'tfc_setpddata'
      'tfc_setweaponbammo'
      'tfc_userkill'
      'TFCMAX_WEAPONS'
      'tickcount'
      'time'
      'tolower'
      'toupper'
      'trace_hull'
      'trace_line'
      'trace_normal'
      'TraceLn'
      'TraceNormal'
      'traceresult'
      'trim'
      'ts_createpwup'
      'ts_getkillingstreak'
      'ts_getusercash'
      'ts_getuseritems'
      'ts_getuserkillflags'
      'ts_getuserlastfrag'
      'ts_getuserpwup'
      'ts_getuserspace'
      'ts_getuserwpn'
      'ts_givepwup'
      'ts_giveweapon'
      'ts_setpddata'
      'ts_wpnlogtoid'
      'ts_wpnlogtoname'
      'TSA_FLASHLIGHT'
      'TSA_LASERSIGHT'
      'TSA_SCOPE'
      'TSA_SILENCER'
      'TSITEM_KUNGFU'
      'TSITEM_SUPERJUMP'
      'TSKF_DOUBLEKILL'
      'TSKF_ISSPEC'
      'TSKF_KILLEDSPEC'
      'TSKF_SLIDINGKILL'
      'TSKF_STUNTKILL'
      'TSMAX_WEAPONS'
      'TSPWUP_ARMOR'
      'TSPWUP_DFIRERATE'
      'TSPWUP_GRENADE'
      'TSPWUP_HEALTH'
      'TSPWUP_INFAMMO'
      'TSPWUP_KUNGFU'
      'TSPWUP_RANDOM'
      'TSPWUP_SLOWMO'
      'TSPWUP_SLOWPAUSE'
      'TSPWUP_SUPERJUMP'
      'ucfirst'
      'unlink'
      'unpause'
      'use'
      'user_has_weapon'
      'user_kill'
      'user_silentkill'
      'user_slap'
      'user_spawn'
      'vaultdata_exists'
      'VecDist'
      'VecLength'
      'VecToAngles'
      'vector_distance'
      'vector_length'
      'vector_to_angle'
      'velocity_by_aim'
      'VelocityByAim'
      'vexd_pfntouch'
      'ViewContents'
      'VOL_NORM'
      'write_angle'
      'write_byte'
      'write_char'
      'write_coord'
      'write_entity'
      'write_file'
      'write_long'
      'write_shortwrite_string'
      'xmod_get_maxweapons'
      'xmod_get_stats_size'
      'xmod_get_wpnlogname'
      'xmod_get_wpnname'
      'xmod_is_custom_wpn'
      'xmod_is_melee_wpn'
      'XS__LIBRELEASE'
      'XS_AMX'
      'XS_AMXX'
      'xvar_exists - BLA blubb XD')
    IgnoreCase = True
    ChooseSingle = False
    AutoHide = True
    DropRestOfWord = False
    CancelAtStart = False
    CompleteWord = False
    CompleteWordOnlyOne = True
    Editor = sciEditor
    WordCharacters = '_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
    Left = 794
    Top = 4
  end
  object cltEditor: TSciCallTips
    ApiStrings.Strings = (
      'access(id, level)'
      'add(dest[], len, const src[], [ max ]) '
      'anglevector(Float:vector[3], FRU, Float:vReturn[3]) '
      'attach_view(player, target) '
      'call_think(entity) '
      'callfunc_begin(const function[], [ const plugin[] ]) '
      'callfunc_begin_i(func, [ plugin ]) '
      'callfunc_end()'
      'callfunc_push_float(Float: value) '
      'callfunc_push_floatrf(Float: value)'
      'callfunc_push_int(value)'
      'callfunc_push_intrf(&value) '
      'callfunc_push_str (value[]) '
      'change_task(id = 0, Float:newTime=1.0, outside = 0)'
      'clamp(value, min=cellmin, max=cellmax)'
      'client_authorized(id)'
      'client_built(index, structure, type, impulse) '
      'client_changeclass(id, newclass, oldclass)'
      'client_changeteam(index, newteam, oldteam)'
      'client_cmd(index, const command[], {Float,...})'
      'client_command(id)'
      'client_connect(id)'
      'client_damage(attacker, victim, damage, wpnindex, hitplace, TA)'
      'client_death(killer, victim, wpnindex, hitplace, TK)'
      'client_disconnect(id) '
      'client_impulse(id, impulse) '
      'client_infochanged(id)'
      'client_kill(id)'
      'client_PostThink(id) '
      'client_PreThink(id) '
      'client_print(index, type, const message[], ...)'
      'client_putinserver(id) '
      'client_spawn(index)'
      'cmd_access(id, level, cid, num) '
      'cmd_target(id, const arg[], flags = 1) '
      'colored_menus()'
      'console_cmd(id, const cmd[], ...)'
      'console_print(id, const message[], ...)'
      'contain(const source[], const string[])'
      'containi(const source[], const string[])'
      'copy(dest[], len, const src[])'
      
        'copy_keyvalue(Classname[], sizeA, Keyname[], sizeB, Value[], siz' +
        'ec)'
      'copyc(dest[], len, const src[], ch)'
      'create_entity(Classname[])'
      'cs_get_hostage_foll(index)'
      'cs_get_hostage_id(index) '
      'cs_get_no_knives()'
      'cs_get_user_bpammo(index, weapon)'
      'cs_get_user_buyzone(index)'
      'cs_get_user_deaths(index)'
      'cs_get_user_defuse(index)'
      'cs_get_user_driving(index)'
      'cs_get_user_hasprim(index)'
      'cs_get_user_model(index, model[], len)'
      'cs_get_user_money(index)'
      'cs_get_user_nvg(index)'
      'cs_get_user_plant(index)'
      'cs_get_user_stationary(index)'
      'cs_get_user_team(index)'
      'cs_get_user_tked(index)'
      'cs_get_user_vip(index)'
      'cs_get_weapon_ammo(index)'
      'cs_get_weapon_burst(index)'
      'cs_get_weapon_id(index)'
      'cs_get_weapon_silen(index)'
      'cs_reset_user_model(index)'
      'cs_set_hostage_foll(index, [ entity ])'
      'cs_set_no_knives([ noknives = 0 ])'
      'cs_set_user_bpammo(index, weapon, amount)'
      'cs_set_user_deaths(index, deaths)'
      
        'cs_set_user_defuse(index, [ defusekit = 1, r = 0, g = 160, b = 0' +
        ', icon[] = "defuser", flash = 0 ])'
      'cs_set_user_model(index, const model[])'
      'cs_set_user_money(index , money, [ flash = 1])'
      'cs_set_user_nvg(index, [ nvgoggles = 1])'
      'cs_set_user_plant(index, [ plant = 1, showbombicon = 1 ])'
      
        'cs_set_user_team(index, CsTeams:team, [ CsInternalModel: model =' +
        ' CS_DONTCHANGE ])'
      'cs_set_user_tked(index, [ tk = 1, subtract = 1 ])'
      'cs_set_user_vip(index, [ vip = 1])'
      'cs_set_weapon_ammo(index, newammo)'
      'cs_set_weapon_burst(index, [ burstmode = 1 ])'
      'cs_set_weapon_silen(index, [ silence = 1 ])'
      'cstrike_running()'
      'custom_weapon_add(name[], melee=0,logname[])'
      'custom_weapon_dmg(weapon, attacker, victim, damage, hitplace=0)'
      'custom_weapon_shot(weapon,player)'
      'cvar_exists(const cvar[])'
      'date([ &year, &month, &day ])'
      'dbi_close(&Sql:sql)'
      
        'dbi_connect(host[], user[], pass[], dbname[], [ error[] = "", ma' +
        'xLength = 0 ])'
      'dbi_error(Sql:sql, error[], maxLength)'
      'dbi_field(Result:result, fieldnum, [ ... ])'
      'dbi_free_result(&Result:result)'
      'dbi_nextrow(Result:result)'
      'dbi_num_rows(result)'
      'dbi_query(Sql:sql, query[], [ ... ])'
      'dbi_result(Result:result, field[], [ ... ])'
      'dbi_type(type[], maxLength)'
      'delete_file(const file[]) '
      'DispatchKeyValue(entity, key[], value[])'
      'DispatchSpawn(entity)'
      'dllfunc(type, [ ... ])'
      'dod_get_map_info(info)'
      'dod_get_next_class(index)'
      'dod_get_pl_deaths(index)'
      'dod_get_pl_teamname(index, szName[], len)'
      'dod_get_pronestate(player)'
      'dod_get_team_score(teamId)'
      'dod_get_user_ammo(index, wid)'
      'dod_get_user_class(index)'
      'dod_get_user_kills(index)'
      'dod_get_user_score(index)'
      'dod_get_user_team(player)'
      'dod_get_user_weapon(index, &clip, &ammo)'
      'dod_is_deployed(index)'
      'dod_is_randomclass(index)'
      
        'dod_set_fuse(index, set = FUSE_SET, Float:newFuse = 5.0, Type = ' +
        'FT_NEW)'
      'dod_set_pl_deaths(index, value, [ refresh = 1 ])'
      'dod_set_pl_teamname(index, szName[])'
      
        'dod_set_stamina(player, set = STAMINA_SET, minvalue = 0, maxvalu' +
        'e = 100)'
      'dod_set_user_ammo(index, wid, value)'
      'dod_set_user_class(index, classId)'
      'dod_set_user_kills(index, value, [ refresh = 1 ])'
      'dod_set_user_score(index, value, [ refresh = 1 ])'
      'dod_set_user_team(index, teamId, [ refresh = 1 ])'
      'dod_user_kill(player)'
      'dod_wpnlog_to_id(logname[])'
      'dod_wpnlog_to_name(logname[], name[], len)'
      'drop_to_floor(entity)'
      
        'emit_sound(index, channel, sample[], Float:vol, Float:att,flags,' +
        ' pitch)'
      'engclient_cmd(index,const command[],arg1[]=)'
      'engclient_print(index, type, const message[], ...)'
      'engfunc(function or type, [ ... ])'
      'entity_count()'
      'entity_get_byte(entity, key)'
      'entity_get_edict(entity, key)'
      'entity_get_float(entity, key)'
      'entity_get_int(entity, key)'
      'entity_get_string(entity, key, Result[], maxLength)'
      'entity_get_vector(entity, key, Float:Vector[3])'
      'entity_range(ida, idb)'
      'entity_set_byte(entity, key, value)'
      'entity_set_edict(entity, key, edict)'
      'entity_set_float(entity, key, Float:value)'
      'entity_set_int(entity, key, value)'
      'entity_set_model(entity, Model[])'
      'entity_set_origin(entity, Float:NewOrigin[3])'
      'entity_set_size(index, Float:mins[3], Float:maxs[3])'
      'entity_set_string(entity, key, const String[])'
      'entity_set_vector(entity, key, Float:NewVector[3])'
      'equal(const a[], const b[], [ c ])'
      'equali(const a[], const b[], [ c ])'
      'fake_touch(Toucher, Touched)'
      'fakedamage(victim, Classname[], Float:damage, damagetype)'
      'fclose(file)'
      'feof(file)'
      'fflush(file)'
      'fgetc(file)'
      'fgetf(file)'
      'fgeti(file)'
      'fgetl(file)'
      'fgets(file)'
      'file_exists(const file[])'
      'file_size(const file[], [ flag ])'
      'find_ent_by_class(StartEntity, Classname[])'
      'find_ent_by_model(StartEntity, Classname[], Model[])'
      
        'find_ent_by_owner(StartIndex, Classname[], OwnerEntity, [ type ]' +
        ')'
      'find_ent_by_target(StartEntity, Classname[])'
      'find_ent_by_tname(StartEntity, Classname[])'
      'find_ent_in_sphere(StartEntity, Float:origin[3], Float:radius)'
      'find_player(const flags[], ...)'
      'find_plugin_bydesc(pdesc[], [ bool:ignorecase=true ])'
      'find_plugin_byfile( pname[], [ bool:ignorecase=true ])'
      
        'find_sphere_class( aroundent, _lookforclassname[], Float:radius,' +
        ' entlist[], maxents, Float:origin[3] = {0.0, 0.0, 0.0})'
      'float(value)'
      'floatabs(Float:value)'
      'floatacos(Float:angle, radix)'
      'floatadd(Float:oper1, Float:oper2)'
      'floatasin(Float:angle, radix)'
      'floatatan(Float:angle, radix)'
      'floatatan2(Float:x, Float:y, radix)'
      'floatcmp(Float:fOne, Float:fTwo)'
      'floatcos(Float:value, [ mode ])'
      'floatdiv(Float:dividend, Float:divisor)'
      'floatfract(Float:value)'
      'floatlog(Float:value, [ Float:base ])'
      'floatmul(Float:oper1, Float:oper2)'
      'floatpower(Float:value, Float:exponent)'
      'floatround(Float:value, [ method ])'
      'floatsin(Float:value, [ mode ])'
      'floatsqroot(Float:value)'
      'floatstr(const string[])'
      'floatsub(Float:oper1, Float:oper2)'
      'floattan(Float:value, [ mode ])'
      'fopen(filename[], mode[])'
      
        'force_unmodified(force_type, mins[3] , maxs[3], const filename[]' +
        ')'
      'force_use(user, used)'
      'format(output[], len, const format[], ...)'
      'format_args(output[], len, [ pos ])'
      'format_time(output[],len, const format[],time = -1)'
      'forward_return(type, [ ... ])'
      'fputc(file, num)'
      'fputf(file, Float:num)'
      'fputi(file, num)'
      'fputl(file, num)'
      'fputs(file, num)'
      'fread(file, ret[], len)'
      'fseek(file, pos, type)'
      'ftell(file)'
      'funcidx(const name[])'
      'FVecIVec(Float:FVec[3], IVec[3])'
      'fwrite(file, const str[], ...)'
      'geoip_code2( ip[], ccode[3])'
      'geoip_code3(ip[], ccode[4])'
      'geoip_country( ip[], result[], [ len = 45 ])'
      'get_basedir(name[], len)'
      'get_brush_entity_origin(entity, Float:Origin[3])'
      'get_class(index)'
      'get_clcmd(index, command[], len1, &flags, info[], len2, flag)'
      'get_clcmdsnum(flag)'
      'get_client_listen(receiver, sender)'
      'get_concmd(index,cmd[],len1,&flags, info[],len2, flag, id = -1)'
      'get_concmdsnum(flag,id = -1)'
      'get_configsdir(name[], len)'
      'get_cvar_flags(const cvar[])'
      'get_cvar_float(const cvarname[])'
      'get_cvar_num(const cvarname[])'
      'get_cvar_string(const cvarname[],output[],iLen)'
      'get_datadir(name[], len)'
      'get_decal_index(const szDecalName[])'
      'get_distance(origin1[3],origin2[3])'
      'get_entity_distance(ent1, ent2)'
      'get_entity_flags(entity)'
      'get_entity_visibility(entity)'
      'get_flags(flags, output[], len)'
      'get_func_id(const funcName[], [ pluginId ])'
      'get_gametime()'
      'get_global_edict(variable)'
      'get_global_float(variable)'
      'get_global_int(variable)'
      'get_global_string(variable, szString[], maxLength)'
      'get_global_vector(variable, Float:vector[3])'
      'get_grenade(player)'
      'get_grenade_id(player, model[], len, grandeid=0)'
      'get_info_keybuffer(entity, buffer[], length)'
      'get_keyvalue(entity, szKey[], value[], maxLength)'
      'get_localinfo(const info[], output[], len)'
      'get_mapname(name[],len)'
      'get_mask(index, mask)'
      'get_maxplayers()'
      'get_maxspeed(index)'
      'get_modname(name[], len)'
      
        'get_module(id, name[], nameLen, author[], authorLen, version[], ' +
        'versionLen, &status)'
      'get_modulesnum()'
      'get_msg_arg_float(argn)'
      'get_msg_arg_int(argn)'
      'get_msg_arg_string(argn, szReturn[], length)'
      'get_msg_args()'
      'get_msg_argtype(argn)'
      'get_msg_block(msgId)'
      'get_msg_origin(Float:Origin[3])'
      'get_offset_char(id, offset, [ linux = 5])'
      'get_offset_float(id, offset, [ linux = 5])'
      'get_offset_int(id, offset, [ linux = 5])'
      'get_offset_short(id, offset, [ linux = 5])'
      'get_pdata_float(index, offset, [ linuxdiff = 5 ])'
      'get_pdata_int(index, offset, [ linuxdiff = 5 ])'
      
        'get_players(players[32], &num, const flags = "", const team = ""' +
        ')'
      'get_playersnum([ flag ])'
      
        'get_plugin(index,filename[],len1,name[],len2,version[],len3,auth' +
        'or[],len4,status[],len5)'
      'get_pluginsnum()'
      'get_res(index)'
      'get_spawn(type, number = 0, Float:ret[3])'
      'get_speak(player)'
      'get_special(index, mask)'
      'get_speed(entity)'
      'get_speedchange(index)'
      'get_srvcmd(index,server_cmd[],len1,&flags, info[],len2, flag)'
      'get_srvcmdsnum(flag)'
      'get_stats(index, stats[8], bodyhits[8], name[], len)'
      'get_stats(player, stats[8], bodyhits[8], name[], len)'
      'get_stats(player, stats[9], bodyhits[8], name[], len)'
      'get_statsnum()'
      'get_statsnum()'
      'get_statsnum()'
      'get_systime(offset = 0)'
      'get_time(const format[],output[],len)'
      'get_timeleft()'
      'get_tr(TraceResult:tr_member, [ ... ])'
      'get_user_aiming(index, &id, &body, [ distance ])'
      'get_user_ammo(index, weapon, &clip, &ammo)'
      'get_user_armor(index)'
      
        'get_user_astats(index, wpnindex, stats[8], bodyhits[8], [ wpnnam' +
        'e = "", len = 0])'
      
        'get_user_astats(index, wpnindex, stats[8], bodyhits[8], [ wpnnam' +
        'e[] = "", len = 0 ])'
      
        'get_user_astats(player, wpnindex, stats[9], bodyhits[8], [ wpnna' +
        'me, maxLength ])'
      'get_user_attacker(index, [ &weapon, &hitzone ])'
      'get_user_authid(index, authid[], len)'
      'get_user_button(player)'
      'get_user_deaths(index)'
      'get_user_flags(index,id=0)'
      'get_user_frags(id)'
      'get_user_godmode(index)'
      'get_user_gravity(index)'
      'get_user_health(id)'
      'get_user_hitzones(index, target)'
      'get_user_index(const name[])'
      'get_user_info(index, const info[], output[], len)'
      'get_user_ip(index, ip[], len, [ without_port ])'
      'get_user_lstats(player, stats[9], bodyhits[8])'
      'get_user_maxspeed(index)'
      'get_user_menu(index,&id,&keys)'
      'get_user_msgid(const name[])'
      'get_user_name(index, name[], len)'
      'get_user_noclip(index)'
      'get_user_oldbutton(player)'
      'get_user_origin(index, origin[3], [ mode ])'
      'get_user_ping(index, &ping, &loss)'
      'get_user_rstats(index, stats[8], bodyhits[8])'
      'get_user_rstats(index, stats[8], bodyhits[8])'
      'get_user_rstats(player, stats[9], bodyhits[8])'
      'get_user_stats(index, stats[8], bodyhits[8])'
      'get_user_stats(index, stats[8], bodyhits[8])'
      'get_user_stats(index, stats[9], bodyhits[8])'
      'get_user_team(index, [ team[], len ])'
      'get_user_time(index, [ flag ])'
      'get_user_userid(index)'
      'get_user_velocity(entity, Float:Vector[3])'
      
        'get_user_vstats(index, victim, stats[8], bodyhits[8], [ wpnname[' +
        '] = "", len = 0 ])'
      
        'get_user_vstats(index, victim, stats[8], bodyhits[8], [ wpnname[' +
        '] = "", len = 0 ])'
      
        'get_user_vstats(player, victim, stats[9], bodyhits[8], [ wpnname' +
        '[], maxLen ])'
      'get_user_weapon(index, &clip, &ammo)'
      'get_user_weapons(index, weapons[32], &num)'
      'get_user_wlstats(index, wpnindex, stats[8], bodyhits[8])'
      'get_user_wlstats(player, wpnindex, stats[9], bodyhits[8])'
      'get_user_wrstats(index, wpnindex, stats[8], bodyhits[8])'
      'get_user_wrstats(index, wpnindex, stats[8], bodyhits[8])'
      'get_user_wrstats(player, wpnindex, stats[9], bodyhits[8])'
      'get_user_wstats(index, wpnindex, stats[8], bodyhits[8])'
      'get_user_wstats(index, wpnindex, stats[8], bodyhits[8])'
      'get_user_wstats(player, wpnindex, stats[9], bodyhits[8])'
      'get_usercmd(type, [ ... ])'
      'get_vaultdata(const key[], [ data[], len ])'
      'get_weaponname(id, weapon[], len)'
      'get_xvar_float(id)'
      'get_xvar_id(const name[])'
      'get_xvar_num(id)'
      'getarg(arg, [ index ])'
      'getkey_float(keytable, key[])'
      'getkey_int(keytable, key[])'
      'getkey_string(keytable, const key[], value[], maxLength)'
      'give_item(index, const item[])'
      'grenade_throw(index, greindex, wId)'
      'grenade_throw(index,greindex,wId)'
      'halflife_time()'
      'has_weapon(index, weapon, [ setweapon = -1])'
      'heapspace()'
      'in_list_float(list, Float:num)'
      'in_list_int(list, num)'
      'in_list_string(list, const str[])'
      'inconsistent_file(id,const filename[], reason[64] )'
      'is_combat()'
      'is_dedicated_server()'
      'is_linux_server()'
      'is_map_valid(const mapname[])'
      'is_module_loaded(const name[])'
      'is_plugin_loaded(const name[])'
      'is_running(const mod[])'
      'is_user_admin(id)'
      'is_user_alive(index)'
      'is_user_bot(index)'
      'is_user_connected(index)'
      'is_user_connecting(index)'
      'is_user_hltv(index)'
      'is_valid_ent(entity)'
      'isalnum(ch)'
      'isalpha(ch)'
      'isdigit(ch)'
      'isspace(ch)'
      'IVecFVec(IVec[3], Float:FVec[3])'
      'jit_enabled()'
      'keytable_clear(keytable)'
      'keytable_count(keytable)'
      'keytable_delete(keytable)'
      'keytable_getkey(keytable, key[], maxLength)'
      'keytable_getval(keytable, val[], maxLength)'
      'keytable_next(keytable)'
      'keytable_reset(keytable)'
      'list_clear_float(list)'
      'list_clear_int(list)'
      'list_clear_string(list)'
      'list_delete_float(list)'
      'list_delete_int(list)'
      'list_delete_string(list)'
      'list_get_float(list, [ position = -1 ])'
      'list_get_int(list, [ position = -1 ])'
      'list_get_string(list, position, value[], maxLength)'
      'list_next_float(list)'
      'list_next_int(list)'
      'list_next_string(list)'
      'list_pop_float(list)'
      'list_pop_int(list)'
      'list_pop_string(list)'
      'list_push_float(list, Float:value)'
      'list_push_int(list, value)'
      'list_push_string(list, position, value[])'
      'list_reset_float(list)'
      'list_reset_int(list)'
      'list_reset_string(list)'
      'list_size_float(list)'
      'list_size_int(list)'
      'list_size_string(list)'
      'list_store_float(list, position, Float:value)'
      'list_store_int(list, position, value)'
      'list_store_string(list, position, value[])'
      'log_amx(const string[], [ ... ])'
      'log_message(const message[], [ ... ])'
      'log_to_file(const file[], const message[], [ ... ])'
      'main(val1, val2)'
      'max(val1, val2)'
      'md5(const szString[], md5buffer[34])'
      'md5_file(const file[], md5buffer[34])'
      'message_begin(dest, msg_type, origin[3]={0,0,0},player=0)'
      'message_end()'
      'new_float_list()'
      'new_int_list()'
      'new_keytable()'
      'new_list_float()'
      'new_list_int()'
      'new_list_string()'
      'new_string_list()'
      'ns_get_build(classname[], [ builtOnly = 1 ], [ Number = 0 ])'
      'ns_get_class(index)'
      'ns_get_deaths(index)'
      'ns_get_energy(index)'
      'ns_get_ent(classname[], value)'
      'ns_get_exp(index)'
      'ns_get_hive_trait(idHive)'
      'ns_get_jpfuel(index)'
      'ns_get_mask(index, mask)'
      'ns_get_maxspeed(index)'
      'ns_get_points(index)'
      'ns_get_res(index)'
      'ns_get_score(index)'
      'ns_get_spawn(team, [ number = 0 ], Float:ret[3])'
      'ns_get_speedchange(index)'
      'ns_get_struct_owner(idStructure)'
      'ns_get_weap_clip(weapon)'
      'ns_get_weap_dmg(weapon)'
      'ns_get_weap_range(weapon)'
      'ns_get_weap_reserve(index, weapon)'
      'ns_give_item(index, const className[])'
      'ns_has_weapon(index, NSWeapon:weapon, [ setweapon = -1 ])'
      'ns_is_combat()'
      'ns_popup(index, szMsg[180], ah)'
      'ns_set_deaths(index, numdeaths)'
      'ns_set_energy(index, Float:energy)'
      'ns_set_exp(index, Float:exp)'
      'ns_set_fov(index, [ Float:fov = 0.0 ])'
      'ns_set_hive_trait(idHive, trait)'
      'ns_set_jpfuel(index, Float:fuel)'
      'ns_set_mask(index, mask, value)'
      
        'ns_set_menu(player, r, g, b, x, y, effects, Float:fadeintime, Fl' +
        'oat:fadeouttime, channel1, channel2)'
      'ns_set_player_body(index, [ body = -1 ])'
      'ns_set_player_model(index, [ model[] = "" ])'
      'ns_set_player_skin(index, [ skin = -1 ])'
      'ns_set_points(index, points)'
      'ns_set_res(index, Float:res)'
      'ns_set_score(index, score)'
      'ns_set_speedchange(index, [ speedchange = 0 ])'
      'ns_set_struct_owner(idStructure, indexOwner)'
      'ns_set_weap_clip(weapon, clipsize)'
      'ns_set_weap_dmg(weapon, Float:damage)'
      'ns_set_weap_range(weapon, Float:range)'
      'ns_set_weap_reserve(index, weapon, ammo)'
      'ns_show_menu(player, command[], text[], keys, time)'
      'ns_spawn_player(player, [ class=1, health=100, armor=25 ])'
      'ns2amx_getammo(index, weapon)'
      'ns2amx_getenergy(index)'
      'ns2amx_gethives()'
      'ns2amx_getjpfuel(index)'
      'ns2amx_giveitem(index, classname[])'
      'ns2amx_isdigesting(index)'
      'ns2amx_nspopup(index, message[190])'
      'ns2amx_setammo(index, weapon, value)'
      'ns2amx_setjpfuel(index, value)'
      'ns2amx_setres(index, value)'
      'num_to_str(num, string[], len)'
      'num_to_word(num, output[], len)'
      'numargs()'
      'parse(const text[], ...)'
      
        'parse_loguser(const text[], name[], nameLen, [ &userid, authid[]' +
        ', authidLen, team[], teamLen ])'
      'parse_time(const input[],const format[], time = -1)'
      'pause(flag[], const param1[]="",const param2[]="")'
      'pev(index, value, [ ... ])'
      'pfn_spawn(entid)'
      'pfn_think(entid)'
      'pfn_touch(ptr, ptd)'
      'pfn_use(user, used)'
      
        'playback_event(flags, invoker, eventindex, Float:delat, Float:or' +
        'igin[3], Float:angles[3], Float:fparam1, Float:fparam2, iparam1,' +
        ' iparam2, bparam1, bparam2)'
      
        'PlaybackEvent(flags, entid, eventid, Float:delay, Float:Origin[3' +
        '], Float:Angles[3], Float:fparam1, Float:fparam2, iparam1, ipara' +
        'm2, bparam2)'
      'plugin_cfg()'
      'plugin_end()'
      'plugin_init()'
      'plugin_log()'
      'plugin_modules()'
      'plugin_pause()'
      'plugin_precache()'
      'plugin_unpause()'
      'PointContents(Float:Origin[3])'
      'precache_event(type, name[], [ ... ])'
      'precache_generic(szFile[])'
      'precache_model(const name[])'
      'precache_sound(const name[])'
      
        'RadiusDamage(Float:ExplodeAt[3], DamageMultiplier, RadiusMultipl' +
        'ier)'
      'random(max)'
      'random_float(Float:a,Float:b)'
      'random_num(a,b)'
      'read_argc()'
      'read_args(output[], len)'
      'read_argv(id, output[], len)'
      'read_data(value, [ ... ])'
      'read_datanum()'
      'read_dir(const dirname[], pos, output[], len, &outLen)'
      'read_file(const file[], line, text[], len, &txtLen)'
      'read_flags(const flags[])'
      'read_logargc(Returns the number of log arguments.)'
      'read_logargv(id, output[], len)'
      'read_logdata(output[], len)'
      'regex_free(&Regex:id)'
      
        'regex_match(const string[], const pattern[], &ret, error[], maxL' +
        'en)'
      'regex_substr(Regex:id, str_id, buffer[], maxLen)'
      
        'register_clcmd(const client_cmd[],const function[],flags=-1, inf' +
        'o[]="")'
      
        'register_concmd(const cmd[],const function[],flags=-1, info[]=""' +
        ')'
      
        'register_cvar(const name[],const string[],flags = 0,Float:fvalue' +
        ' = 0.0)'
      'register_dictionary(const file[])'
      
        'register_event(const event[], const function[], const flags[], [' +
        ' cond=[], ... ])'
      'register_forward(forwardType, callback[], [ post = 0 ])'
      'register_impulse(impulse, function[])'
      'register_logevent(const function[], argsnum, ...)'
      'register_menu(title[], keys, function[], outside=0)'
      'register_menucmd(menuid,keys, const function[])'
      'register_menuid(const menu[], outside=0)'
      'register_message(msgId, function[])'
      
        'register_plugin(const plugin_name[], const version[], const auth' +
        'or[])'
      
        'register_srvcmd(const server_cmd[],const function[],flags=-1, in' +
        'fo[]="")'
      'register_statsfwd(forward_index)'
      'register_think(Classname[], function[])'
      'register_touch(Toucher[], Touched[], function[])'
      'remove_cvar_flags(const cvar[],flags = -1)'
      'remove_entity(entity)'
      'remove_entity_name(Name[])'
      'remove_quotes(text[])'
      'remove_task(id = 0, outside = 0)'
      'remove_user_flags(index,flags=-1,id=0)'
      'remove_vaultdata(const key[])'
      'replace(text[], len, const what[], const with[])'
      'require_module(const name[])'
      'reset_user_wstats(index)'
      'reset_user_wstats(index)'
      'reset_user_wstats(player)'
      'rewind(file)'
      'server_changelevel(map[])'
      'server_cmd(const command[],{Float,_}:...)'
      'server_exec()'
      'server_frame()'
      'server_print(const message[], [ ... ])'
      'set_client_listen(receiver, sender, listen)'
      'set_cvar_flags(const cvar[],flags)'
      'set_cvar_float(const cvar[],Float:value)'
      'set_cvar_num(const cvarname[],value)'
      'set_cvar_string(const cvar[],const value[]))'
      'set_entity_flags(entity, flag, onoff)'
      'set_entity_visibility(entity, [ visible = 1 ])'
      
        'set_hudmessage(red=200, green=100, blue=0, Float:x=-1.0, Float:y' +
        '=0.35, effects=0, Float:fxtime=6.0, Float:holdtime=12.0, Float:f' +
        'adeintime=0.1, Float:fadeouttime=0.2, channel=4)'
      'set_lights(const Lighting[])'
      'set_localinfo(const info[], const value[])'
      'set_mask(index, mask, value)'
      'set_msg_arg_float(argn, argtype, Float:fValue)'
      'set_msg_arg_int(argn, argtype, iValue)'
      'set_msg_arg_string(argn, szString[])'
      'set_msg_block(msgId, flag)'
      'set_offset_char(id, offset, value, [ linux = 5 ])'
      'set_offset_float(id, offset, Float:value, [ linux = 5 ])'
      'set_offset_int(id, offset, value, [ linux = 5 ])'
      'set_offset_short(id, offset, value, [ linux = 5 ])'
      'set_pdata_float(index, offset, Float:value, [ linuxdiff = -5 ])'
      'set_pdata_int(index, offset, value, [ linuxdiff = -5 ])'
      'set_pev(index, value, [ ... ])'
      'set_player_body(index, [ body = -1 ])'
      'set_player_model(index, [ model = "" ])'
      'set_player_skin(index, [ skin = -1 ])'
      
        'set_rendering(index, fx=kRenderFxNone, r=255,g=255,b=255, render' +
        '=kRenderNormal,amount=16)'
      'set_speak(player, speakFlags)'
      'set_speedchange(index, speed)'
      
        'set_task(Float:time,const function[],id = 0,parameter[]="",len =' +
        ' 0,flags[]="", repeat = 0)'
      'set_tr(TraceResult:tr_member, [ ... ])'
      'set_user_armor(index, armor)'
      'set_user_deaths(index, newDeaths)'
      'set_user_flags(index,flags=-1,id=0)'
      'set_user_footsteps(id, [ set = 1 ])'
      'set_user_frags(index, newFrags)'
      'set_user_godmode(index, [ godmode = 0 ])'
      'set_user_gravity(index, [ Float:gravity = 1.0 ])'
      'set_user_health(index, health)'
      'set_user_hitzones([ index = 0, target = 0, body = 255 [)'
      'set_user_info(index, const info[], const value[])'
      'set_user_maxspeed(index, [ Float:speed = - 1.0 ])'
      'set_user_noclip(index, [ noclip = 0 ])'
      'set_user_origin(index, origin[3])'
      
        'set_user_rendering(index, [ fx = kRenderFxNone, r = 255, g = 255' +
        ', b = 255, render = kRenderNormal, amount = 16 ])'
      'set_user_velocity(entity, Float:Vector[3])'
      'set_usercmd(type, [ ... ])'
      'set_vaultdata(const key[], [ const data[] ])'
      'set_view(player, viewtype)'
      'set_xvar_float(id, Float:value = 0.0)'
      'set_xvar_num(id, value = 0 )'
      'setarg(arg, index=0, value)'
      'setc(src[], len, ch)'
      'show_hudmessage(index, message[], ...)'
      'show_menu(index, keys, const menu[], time = -1, title[] = "")'
      'show_motd(player, const message[], [ const header[] ])'
      'socket_change(socket, [ timeout = 100000 ])'
      'socket_close(socket)'
      'socket_open(hostname[], port, protocol, &error)'
      'socket_recv(socket, data[], maxLength)'
      'socket_send(socket, data[], length)'
      'spawn(index)'
      'str_to_num(const string[])'
      'strbreak(const text[], left[], leftLen, right[], rightLen)'
      'strip_user_weapons(index)'
      'strlen(const string[])'
      'strpack(dest[], const source[])'
      'strtolower(string[])'
      'strtoupper(string[])'
      'strunpack(dest[], const source[])'
      'swapchars(c)'
      'takedamage(victim, inflictor, attacker, Float:damage, type)'
      'task_exists(id = 0, outside = 0)'
      'tfc_clearmodel(index)'
      'tfc_getbammo(index, ammo)'
      'tfc_getgrenadename(weapon, name[], len)'
      'tfc_getweaponbammo(player, weapon)'
      'tfc_getwpnlogname(weapon, name[], len)'
      'tfc_getwpnname(weapon, name[], len)'
      'tfc_isgrenade(weapon)'
      'tfc_ismelee(weapon)'
      'tfc_setbammo(index, ammo, value)'
      'tfc_setmodel(index, const Model[], const Skin[])'
      
        'tfc_setpddata(timer, sentrygun, shells, bullets, cells, rockets,' +
        ' nade1, nade2)'
      'tfc_setweaponbammo(player, weapon, value)'
      'tfc_userkill(index)'
      'tickcount([ &granularity ])'
      'time([ &hour, &minute, &second ])'
      'tolower(c)'
      'toupper(c)'
      
        'trace_hull(Float:origin[3], hull, [ ignoredent=0, ignoremonsters' +
        '=0 ])'
      
        'trace_line(IgnoreEnt, Float:Start[3], Float:End[3], Float:vRetur' +
        'n[3])'
      'traceresult(type, [ ... ])'
      'trim(text[])'
      'ts_createpwup(pwup)'
      'ts_getkillingstreak(index)'
      'ts_getusercash(index)'
      'ts_getuseritems(index)'
      'ts_getuserkillflags(killer)'
      'ts_getuserlastfrag(index)'
      'ts_getuserpwup(index, &Value)'
      'ts_getuserspace(index)'
      'ts_getuserwpn(index, &ammo, &clip, &mode, &extra)'
      'ts_givepwup(index, pwupent)'
      'ts_giveweapon(index, weapon, clips, extra)'
      'ts_setpdata(knifeoffset)'
      'ts_wpnlogtoid(logname[])'
      'ts_wpnlogtoname(logname[], name[], len)'
      'ucfirst(string[])'
      'unlink(const filename[], ...)'
      'unpause(flag[], const param1[]="",const param2[]="")'
      'user_has_weapon(index, weapon, [ setweapon ])'
      'user_kill(index, [ flag ])'
      'user_silentkill(player)'
      'user_slap(index, power, [ rnddir ])'
      'vaultdata_exists(const key[])'
      'vector_distance(Float:Vector1[3], Float:Vector2[3])'
      'vector_length(Float:Vector[3])'
      'vector_to_angle(Float:vector[3], Float:vReturn[3])'
      'VelocityByAim(entity, velocity, Float:RetValue[3])'
      'ViewContents(player)'
      'write_angle(x)'
      'write_byte(x)'
      'write_char(x)'
      'write_coord(x)'
      'write_entity(x)'
      'write_file(const file[], const text[], [ line ])'
      'write_long(x)'
      'write_short(x)'
      'write_string(x)'
      'xmod_get_maxweapons()'
      'xmod_get_stats_size()'
      'xmod_get_wpnlogname(wpnindex, name[], len)'
      'xmod_get_wpnname(wpnindex, name[], len)'
      'xmod_is_custom_wpn(wpnindex)'
      'xmod_is_melee_wpn(wpnindex)'
      'xvar_exists(const name[])'
      'zdod_get_next_class(player)'
      'zdod_get_pl_deaths(player)'
      'zdod_is_randomclass(player)'
      
        'zdod_set_fuse(player, [ set=FUSE_SET, float:newFuse=5.0, type=FT' +
        '_NEW ])'
      'zdod_set_pl_deaths(player, value, [ refresh=1 ])'
      'zdod_set_pl_teamname(player, szName[])'
      
        'zdod_set_stamina(player, [ set=STAMINA_SET, minvalue=0, maxvalue' +
        '=100 ])'
      'zdod_set_user_class(player, classId)'
      'zdod_set_user_score(player, value, [ refresh=1 ])'
      'zdod_set_user_team(player, teamId, [ refresh=1 ])')
    EndDefinition = ')'
    Editor = sciEditor
    ParametersEnd = ')'
    ParametersSeparators = ','
    ParametersStart = '('
    WordCharacters = '_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
    IgnoreCase = True
    Left = 794
    Top = 34
  end
  object srpSearch: TSciSearchReplace
    SearchForSelWord = False
    Editor = sciEditor
    Left = 762
    Top = 34
  end
  object odOpen: TOpenDialog
    Filter = 
      'SMA-Files (*.sma)|*.sma|INC-Files (*.inc)|*.inc|All files (*.*)|' +
      '*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open...'
    Left = 730
    Top = 4
  end
  object sdSave: TSaveDialog
    Filter = 
      'SMA-Files (*.sma)|*.sma|INC-Files (*.inc)|*.inc|All files (*.*)|' +
      '*.*'
    Title = 'Save...'
    Left = 730
    Top = 34
  end
  object ppmEditor: TPopupMenu
    Left = 762
    Top = 4
    object mnuClose: TMenuItem
      Caption = 'Close'
      Enabled = False
      OnClick = mnuCloseClick
    end
  end
  object IdFTP: TIdFTP
    MaxLineAction = maException
    ProxySettings.ProxyType = fpcmNone
    ProxySettings.Port = 0
    Left = 698
    Top = 4
  end
end
