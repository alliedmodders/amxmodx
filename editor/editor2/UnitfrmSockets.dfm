object frmSocketTerminal: TfrmSocketTerminal
  Left = 192
  Top = 110
  ActiveControl = rtfEnter
  BorderStyle = bsDialog
  Caption = 'Socket Terminal'
  ClientHeight = 230
  ClientWidth = 324
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object pnlSettings: TPanel
    Left = 0
    Top = 141
    Width = 324
    Height = 89
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object lblStatusCaption: TLabel
      Left = 2
      Top = 72
      Width = 35
      Height = 13
      Caption = 'Status:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblStatus: TLabel
      Left = 40
      Top = 72
      Width = 69
      Height = 13
      Caption = 'not connected'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblSettings: TLabel
      Left = 4
      Top = 6
      Width = 43
      Height = 13
      Caption = 'Settings:'
    end
    object pnlSettings2: TPanel
      Left = 4
      Top = 24
      Width = 311
      Height = 41
      BevelOuter = bvLowered
      TabOrder = 0
      object lblHost: TLabel
        Left = 4
        Top = 3
        Width = 26
        Height = 13
        Caption = 'Host:'
      end
      object lblPort: TLabel
        Left = 130
        Top = 3
        Width = 24
        Height = 13
        Caption = 'Port:'
      end
      object txtHost: TFlatEdit
        Left = 4
        Top = 17
        Width = 121
        Height = 19
        ColorFlat = clBtnFace
        ParentColor = True
        TabOrder = 0
      end
      object txtPort: TFlatEdit
        Left = 130
        Top = 17
        Width = 39
        Height = 19
        ColorFlat = clBtnFace
        ParentColor = True
        TabOrder = 1
        Text = '1'
        OnChange = txtPortChange
      end
      object optUDP: TFlatRadioButton
        Left = 176
        Top = 20
        Width = 39
        Height = 17
        Caption = 'UDP'
        TabOrder = 3
        OnClick = optTCPClick
      end
      object optTCP: TFlatRadioButton
        Left = 176
        Top = 4
        Width = 35
        Height = 15
        Caption = 'TCP'
        Checked = True
        TabOrder = 2
        TabStop = True
        OnClick = optTCPClick
      end
      object cmdConnect: TFlatButton
        Left = 224
        Top = 10
        Width = 77
        Height = 21
        ColorHighLight = 8623776
        ColorShadow = 8623776
        Caption = 'Connect'
        TabOrder = 4
        OnClick = cmdConnectClick
      end
    end
  end
  object rtfEnter: TRichEdit
    Left = 0
    Top = 121
    Width = 324
    Height = 20
    Align = alBottom
    TabOrder = 1
    WantReturns = False
    OnKeyPress = rtfEnterKeyPress
  end
  object rtfReceived: TRichEdit
    Left = 0
    Top = 0
    Width = 324
    Height = 121
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object IdTCPClient: TIdTCPClient
    MaxLineAction = maSplit
    OnDisconnected = IdTCPClientDisconnected
    OnConnected = IdTCPClientConnected
    Port = 0
    Left = 4
    Top = 4
  end
  object IdUDPClient: TIdUDPClient
    OnStatus = IdUDPClientStatus
    Port = 0
    Left = 4
    Top = 34
  end
  object alCopyPaste: TActionList
    Left = 4
    Top = 64
    object acCopy: TAction
      Caption = 'Copy'
      ShortCut = 16451
      OnExecute = acCopyExecute
    end
    object acPaste: TAction
      Caption = 'Paste'
      ShortCut = 16470
      OnExecute = acPasteExecute
    end
    object acUndo: TAction
      Caption = 'Undo'
      OnExecute = acUndoExecute
    end
    object acSelectAll: TAction
      Caption = 'Select all'
      OnExecute = acSelectAllExecute
    end
  end
end
