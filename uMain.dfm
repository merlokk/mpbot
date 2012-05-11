object MainFrm: TMainFrm
  Left = 0
  Top = 0
  Caption = 'MPolis2'
  ClientHeight = 599
  ClientWidth = 696
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
    Width = 696
    Height = 599
    ActivePage = tsEditor
    Align = alClient
    TabOrder = 0
    object tsEditor: TTabSheet
      Caption = 'Editor'
      object Label1: TLabel
        Left = 16
        Top = 27
        Width = 76
        Height = 16
        Caption = 'VK username'
      end
      object Label2: TLabel
        Left = 16
        Top = 57
        Width = 74
        Height = 16
        Caption = 'VK password'
      end
      object Label3: TLabel
        Left = 16
        Top = 87
        Width = 38
        Height = 16
        Caption = 'VK key'
      end
      object Label4: TLabel
        Left = 14
        Top = 147
        Width = 96
        Height = 16
        Caption = 'VK application ID'
      end
      object Label6: TLabel
        Left = 444
        Top = 27
        Width = 44
        Height = 16
        Caption = 'revision'
      end
      object Label7: TLabel
        Left = 444
        Top = 57
        Width = 94
        Height = 16
        Caption = 'get user stat ver'
      end
      object Label8: TLabel
        Left = 408
        Top = 87
        Width = 129
        Height = 16
        Caption = 'check and perform ver'
      end
      object Label9: TLabel
        Left = 348
        Top = 117
        Width = 91
        Height = 16
        Caption = 'flash player_ver'
      end
      object Label10: TLabel
        Left = 382
        Top = 193
        Width = 36
        Height = 16
        Caption = 'count:'
      end
      object Label5: TLabel
        Left = 16
        Top = 117
        Width = 60
        Height = 16
        Caption = 'VK user ID'
      end
      object Label11: TLabel
        Left = 344
        Top = 149
        Width = 74
        Height = 16
        Caption = 'App auth key'
      end
      object edVKUser: TEdit
        Left = 123
        Top = 24
        Width = 182
        Height = 24
        TabOrder = 0
        Text = 'ol@lot.kharkov.ua'
      end
      object edVKPasswd: TEdit
        Left = 123
        Top = 54
        Width = 182
        Height = 24
        PasswordChar = '*'
        TabOrder = 1
        Text = 'close%20all'
      end
      object edVKAccessKey: TEdit
        Left = 123
        Top = 84
        Width = 182
        Height = 24
        TabOrder = 2
        Text = 'ET8HhOAeuOF4nDfS7GnT'
      end
      object edVKAppID: TEdit
        Left = 123
        Top = 141
        Width = 121
        Height = 24
        TabOrder = 3
        Text = '1858070'
      end
      object btParamSave: TButton
        Left = 517
        Top = 232
        Width = 75
        Height = 25
        Caption = 'Save'
        TabOrder = 4
      end
      object btParamLoad: TButton
        Left = 598
        Top = 232
        Width = 75
        Height = 25
        Caption = 'Load'
        TabOrder = 5
      end
      object btInit: TButton
        Left = 111
        Top = 303
        Width = 75
        Height = 25
        Caption = 'Init'
        TabOrder = 6
        OnClick = btInitClick
      end
      object btRun: TButton
        Left = 192
        Top = 303
        Width = 75
        Height = 25
        Caption = 'Run'
        TabOrder = 7
        OnClick = btRunClick
      end
      object cbAutorun: TCheckBox
        Left = 23
        Top = 307
        Width = 73
        Height = 17
        Caption = 'autorun'
        TabOrder = 8
      end
      object btLoadDatrabase: TButton
        Left = 16
        Top = 334
        Width = 251
        Height = 25
        Caption = 'Update game items database'
        TabOrder = 9
        OnClick = btLoadDatrabaseClick
      end
      object edRevision: TEdit
        Left = 544
        Top = 24
        Width = 64
        Height = 24
        TabOrder = 10
        Text = 'f86c43'
      end
      object edVerStat: TEdit
        Left = 544
        Top = 54
        Width = 64
        Height = 24
        TabOrder = 11
        Text = '1'
      end
      object edVerCheck: TEdit
        Left = 544
        Top = 84
        Width = 64
        Height = 24
        TabOrder = 12
        Text = '5'
      end
      object edVerFPStat: TEdit
        Left = 445
        Top = 114
        Width = 64
        Height = 24
        TabOrder = 13
        Text = '11'
      end
      object edVerFP: TEdit
        Left = 517
        Top = 114
        Width = 156
        Height = 24
        TabOrder = 14
        Text = 'WIN%2011,1,102,63'
      end
      object cbNeedGifts: TCheckBox
        Left = 15
        Top = 194
        Width = 113
        Height = 17
        Caption = 'gifts needed id:'
        TabOrder = 15
      end
      object edNeedGiftId: TEdit
        Left = 134
        Top = 187
        Width = 242
        Height = 24
        TabOrder = 16
        Text = 'gift_for_christmas_tree2'
      end
      object edNeedGiftCount: TEdit
        Left = 424
        Top = 187
        Width = 64
        Height = 24
        TabOrder = 17
        Text = '30'
      end
      object edVKUserID: TEdit
        Left = 123
        Top = 114
        Width = 121
        Height = 24
        TabOrder = 18
        Text = '56895991'
      end
      object edAppAuthKey: TEdit
        Left = 424
        Top = 144
        Width = 249
        Height = 24
        TabOrder = 19
        Text = '3b4fd2ca7f718dd8a07f28c4170673c1'
      end
      object btDownloadSWF: TButton
        Left = 16
        Top = 365
        Width = 251
        Height = 25
        Caption = 'Download swf file'
        TabOrder = 20
        OnClick = btDownloadSWFClick
      end
    end
    object tsLog: TTabSheet
      Caption = 'Log'
      ImageIndex = 1
      DesignSize = (
        688
        568)
      object lbLog: TListBox
        Left = 3
        Top = 67
        Width = 682
        Height = 446
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
      end
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 328
    Top = 24
  end
end
