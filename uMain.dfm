object MainFrm: TMainFrm
  Left = 0
  Top = 0
  Caption = 'MPolis2'
  ClientHeight = 599
  ClientWidth = 733
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 733
    Height = 599
    ActivePage = tsEditor
    Align = alClient
    TabOrder = 0
    object tsEditor: TTabSheet
      Caption = 'Editor'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label1: TLabel
        Left = 16
        Top = 59
        Width = 76
        Height = 16
        Caption = 'VK username'
      end
      object Label2: TLabel
        Left = 16
        Top = 89
        Width = 74
        Height = 16
        Caption = 'VK password'
      end
      object Label3: TLabel
        Left = 16
        Top = 119
        Width = 38
        Height = 16
        Caption = 'VK key'
      end
      object Label4: TLabel
        Left = 368
        Top = 179
        Width = 96
        Height = 16
        Caption = 'VK application ID'
      end
      object Label6: TLabel
        Left = 468
        Top = 59
        Width = 44
        Height = 16
        Caption = 'revision'
      end
      object Label7: TLabel
        Left = 468
        Top = 89
        Width = 94
        Height = 16
        Caption = 'get user stat ver'
      end
      object Label8: TLabel
        Left = 432
        Top = 119
        Width = 129
        Height = 16
        Caption = 'check and perform ver'
      end
      object Label9: TLabel
        Left = 372
        Top = 149
        Width = 91
        Height = 16
        Caption = 'flash player_ver'
      end
      object Label5: TLabel
        Left = 16
        Top = 149
        Width = 60
        Height = 16
        Caption = 'VK user ID'
      end
      object Label11: TLabel
        Left = 14
        Top = 179
        Width = 74
        Height = 16
        Caption = 'App auth key'
      end
      object Label15: TLabel
        Left = 14
        Top = 27
        Width = 74
        Height = 16
        Caption = 'Param group'
      end
      object edVKUser: TEdit
        Left = 100
        Top = 56
        Width = 182
        Height = 24
        TabOrder = 0
      end
      object edVKPasswd: TEdit
        Left = 100
        Top = 86
        Width = 182
        Height = 24
        PasswordChar = '*'
        TabOrder = 1
      end
      object edVKAccessKey: TEdit
        Left = 100
        Top = 116
        Width = 182
        Height = 24
        TabOrder = 2
      end
      object edVKAppID: TEdit
        Left = 470
        Top = 176
        Width = 121
        Height = 24
        TabOrder = 3
        Text = '1858070'
      end
      object btParamSave: TButton
        Left = 517
        Top = 264
        Width = 75
        Height = 25
        Caption = 'Save'
        TabOrder = 4
        OnClick = btParamSaveClick
      end
      object btParamLoad: TButton
        Left = 598
        Top = 264
        Width = 75
        Height = 25
        Caption = 'Load'
        TabOrder = 5
        OnClick = btParamLoadClick
      end
      object btInit: TButton
        Left = 110
        Top = 343
        Width = 75
        Height = 25
        Caption = 'Init'
        TabOrder = 6
        OnClick = btInitClick
      end
      object btRun: TButton
        Left = 191
        Top = 343
        Width = 75
        Height = 25
        Caption = 'Run'
        TabOrder = 7
        OnClick = btRunClick
      end
      object cbAutorun: TCheckBox
        Left = 22
        Top = 347
        Width = 73
        Height = 17
        Caption = 'autorun'
        TabOrder = 8
      end
      object btLoadDatrabase: TButton
        Left = 15
        Top = 374
        Width = 251
        Height = 25
        Caption = 'Update game items database'
        TabOrder = 9
        OnClick = btLoadDatrabaseClick
      end
      object edRevision: TEdit
        Left = 568
        Top = 56
        Width = 64
        Height = 24
        TabOrder = 10
        Text = 'f86c43'
      end
      object edVerStat: TEdit
        Left = 568
        Top = 86
        Width = 64
        Height = 24
        TabOrder = 11
        Text = '1'
      end
      object edVerCheck: TEdit
        Left = 568
        Top = 116
        Width = 64
        Height = 24
        TabOrder = 12
        Text = '5'
      end
      object edVerFPStat: TEdit
        Left = 469
        Top = 146
        Width = 64
        Height = 24
        TabOrder = 13
      end
      object edVerFP: TEdit
        Left = 541
        Top = 146
        Width = 156
        Height = 24
        TabOrder = 14
      end
      object cbNeedGifts: TCheckBox
        Left = 15
        Top = 226
        Width = 113
        Height = 17
        Caption = 'gifts needed id:'
        TabOrder = 15
      end
      object edNeedGiftId: TEdit
        Left = 134
        Top = 219
        Width = 539
        Height = 24
        TabOrder = 16
        Text = 'gift_for_christmas_tree2:30'
      end
      object edVKUserID: TEdit
        Left = 100
        Top = 146
        Width = 121
        Height = 24
        TabOrder = 17
      end
      object edAppAuthKey: TEdit
        Left = 100
        Top = 176
        Width = 249
        Height = 24
        TabOrder = 18
      end
      object btDownloadSWF: TButton
        Left = 15
        Top = 405
        Width = 251
        Height = 25
        Caption = 'Download swf file'
        TabOrder = 19
        OnClick = btDownloadSWFClick
      end
      object cbParamGroup: TComboBox
        Left = 100
        Top = 26
        Width = 222
        Height = 24
        Style = csDropDownList
        TabOrder = 20
        OnChange = cbParamGroupChange
      end
    end
    object tsLog: TTabSheet
      Caption = 'Log'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        725
        568)
      object Label12: TLabel
        Left = 3
        Top = 16
        Width = 29
        Height = 16
        Caption = 'Level'
      end
      object lbLevel: TLabel
        Left = 48
        Top = 16
        Width = 15
        Height = 16
        Caption = '---'
      end
      object Label14: TLabel
        Left = 3
        Top = 38
        Width = 37
        Height = 16
        Caption = 'Money'
      end
      object lbMoney: TLabel
        Left = 46
        Top = 38
        Width = 15
        Height = 16
        Caption = '---'
      end
      object Label10: TLabel
        Left = 171
        Top = 16
        Width = 24
        Height = 16
        Caption = 'Fuel'
      end
      object lbFuel: TLabel
        Left = 201
        Top = 16
        Width = 15
        Height = 16
        Caption = '---'
      end
      object imGraph: TImage
        Left = 3
        Top = 519
        Width = 715
        Height = 46
        ParentCustomHint = False
        Anchors = [akLeft, akBottom]
        ParentShowHint = False
        ShowHint = True
      end
      object Label13: TLabel
        Left = 689
        Top = 3
        Width = 33
        Height = 16
        Caption = 'Room'
      end
      object lbCurRoom: TLabel
        Left = 689
        Top = 25
        Width = 33
        Height = 24
        Alignment = taCenter
        AutoSize = False
        Caption = '---'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -20
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lbLog: TListBox
        Left = 3
        Top = 67
        Width = 719
        Height = 446
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
      end
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 352
    Top = 24
  end
end
