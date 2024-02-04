object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 705
  ClientWidth = 1032
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object lblAge: TLabel
    Left = 20
    Top = 20
    Width = 24
    Height = 15
    Caption = 'Age:'
  end
  object lblBirthDate: TLabel
    Left = 20
    Top = 50
    Width = 55
    Height = 15
    Caption = 'Birth Date:'
  end
  object lblBloodGroup: TLabel
    Left = 20
    Top = 79
    Width = 70
    Height = 15
    Caption = 'Blood Group:'
  end
  object lblDomain: TLabel
    Left = 20
    Top = 108
    Width = 45
    Height = 15
    Caption = 'Domain:'
  end
  object lblEin: TLabel
    Left = 20
    Top = 169
    Width = 19
    Height = 15
    Caption = 'Ein:'
  end
  object lblEmail: TLabel
    Left = 20
    Top = 168
    Width = 32
    Height = 15
    Caption = 'Email:'
  end
  object lblEyeColor: TLabel
    Left = 20
    Top = 198
    Width = 53
    Height = 15
    Caption = 'Eye Color:'
  end
  object lblFirstName: TLabel
    Left = 20
    Top = 228
    Width = 60
    Height = 15
    Caption = 'First Name:'
  end
  object lblGender: TLabel
    Left = 20
    Top = 258
    Width = 41
    Height = 15
    Caption = 'Gender:'
  end
  object lblHair: TLabel
    Left = 20
    Top = 319
    Width = 25
    Height = 15
    Caption = 'Hair:'
  end
  object lblHeight: TLabel
    Left = 20
    Top = 318
    Width = 39
    Height = 15
    Caption = 'Height:'
  end
  object lblId: TLabel
    Left = 20
    Top = 379
    Width = 13
    Height = 15
    Caption = 'Id:'
  end
  object lblImage: TLabel
    Left = 20
    Top = 378
    Width = 36
    Height = 15
    Caption = 'Image:'
  end
  object lblIp: TLabel
    Left = 20
    Top = 439
    Width = 13
    Height = 15
    Caption = 'Ip:'
  end
  object lblLastName: TLabel
    Left = 20
    Top = 438
    Width = 59
    Height = 15
    Caption = 'Last Name:'
  end
  object lblMacAddress: TLabel
    Left = 20
    Top = 468
    Width = 71
    Height = 15
    Caption = 'Mac Address:'
  end
  object lblMaidenName: TLabel
    Left = 20
    Top = 498
    Width = 78
    Height = 15
    Caption = 'Maiden Name:'
  end
  object lblPhone: TLabel
    Left = 20
    Top = 527
    Width = 37
    Height = 15
    Caption = 'Phone:'
  end
  object lblSsn: TLabel
    Left = 20
    Top = 588
    Width = 21
    Height = 15
    Caption = 'Ssn:'
  end
  object lblUniversity: TLabel
    Left = 20
    Top = 587
    Width = 55
    Height = 15
    Caption = 'University:'
  end
  object lblUserAgent: TLabel
    Left = 20
    Top = 617
    Width = 61
    Height = 15
    Caption = 'User Agent:'
  end
  object lblWeight: TLabel
    Left = 20
    Top = 646
    Width = 41
    Height = 15
    Caption = 'Weight:'
  end
  object LabelEin: TLabel
    Left = 20
    Top = 141
    Width = 16
    Height = 15
    Caption = 'Ein'
  end
  object LabelHair: TLabel
    Left = 20
    Top = 291
    Width = 22
    Height = 15
    Caption = 'Hair'
  end
  object LabelId: TLabel
    Left = 20
    Top = 351
    Width = 10
    Height = 15
    Caption = 'Id'
  end
  object LabelIp: TLabel
    Left = 20
    Top = 411
    Width = 13
    Height = 15
    Caption = 'Ip:'
  end
  object LabelSsn: TLabel
    Left = 20
    Top = 561
    Width = 21
    Height = 15
    Caption = 'Ssn:'
  end
  object edtAge: TEdit
    Left = 120
    Top = 20
    Width = 300
    Height = 23
    TabOrder = 0
    Text = '0'
  end
  object edtBirthDate: TEdit
    Left = 120
    Top = 50
    Width = 300
    Height = 23
    TabOrder = 1
    Text = '01/01/2000'
  end
  object edtBloodGroup: TEdit
    Left = 120
    Top = 79
    Width = 300
    Height = 23
    TabOrder = 2
  end
  object edtDomain: TEdit
    Left = 120
    Top = 108
    Width = 300
    Height = 23
    TabOrder = 3
  end
  object edtEin: TEdit
    Left = 120
    Top = 138
    Width = 300
    Height = 23
    TabOrder = 4
  end
  object edtEmail: TEdit
    Left = 120
    Top = 168
    Width = 300
    Height = 23
    TabOrder = 5
  end
  object edtEyeColor: TEdit
    Left = 120
    Top = 198
    Width = 300
    Height = 23
    TabOrder = 6
  end
  object edtFirstName: TEdit
    Left = 120
    Top = 228
    Width = 300
    Height = 23
    TabOrder = 7
  end
  object edtGender: TEdit
    Left = 120
    Top = 258
    Width = 300
    Height = 23
    TabOrder = 8
  end
  object edtHair: TEdit
    Left = 120
    Top = 288
    Width = 300
    Height = 23
    TabOrder = 9
  end
  object edtHeight: TEdit
    Left = 120
    Top = 318
    Width = 300
    Height = 23
    TabOrder = 10
    Text = '0'
  end
  object edtId: TEdit
    Left = 120
    Top = 348
    Width = 300
    Height = 23
    TabOrder = 11
    Text = '0'
  end
  object edtImage: TEdit
    Left = 120
    Top = 378
    Width = 300
    Height = 23
    TabOrder = 12
  end
  object edtIp: TEdit
    Left = 120
    Top = 408
    Width = 300
    Height = 23
    TabOrder = 13
  end
  object edtLastName: TEdit
    Left = 120
    Top = 438
    Width = 300
    Height = 23
    TabOrder = 14
  end
  object edtMacAddress: TEdit
    Left = 120
    Top = 468
    Width = 300
    Height = 23
    TabOrder = 15
  end
  object edtMaidenName: TEdit
    Left = 120
    Top = 498
    Width = 300
    Height = 23
    TabOrder = 16
  end
  object edtPhone: TEdit
    Left = 120
    Top = 527
    Width = 300
    Height = 23
    TabOrder = 17
  end
  object edtSsn: TEdit
    Left = 120
    Top = 557
    Width = 300
    Height = 23
    TabOrder = 18
  end
  object edtUniversity: TEdit
    Left = 120
    Top = 587
    Width = 300
    Height = 23
    TabOrder = 19
  end
  object edtUserAgent: TEdit
    Left = 120
    Top = 617
    Width = 300
    Height = 23
    TabOrder = 20
  end
  object edtWeight: TEdit
    Left = 120
    Top = 646
    Width = 300
    Height = 23
    TabOrder = 21
    Text = '0'
  end
  object Button1: TButton
    Left = 949
    Top = 675
    Width = 75
    Height = 25
    Action = acUsertNext
    TabOrder = 22
  end
  object Button2: TButton
    Left = 866
    Top = 675
    Width = 75
    Height = 25
    Action = actUserPrevious
    TabOrder = 23
  end
  object PageControl1: TPageControl
    Left = 426
    Top = 20
    Width = 598
    Height = 649
    ActivePage = TabSheet1
    TabOrder = 24
    object TabSheet1: TTabSheet
      Caption = 'Products'
      object LabelTitle: TLabel
        Left = 8
        Top = 63
        Width = 25
        Height = 15
        Caption = 'Title:'
      end
      object LabelThumbnail: TLabel
        Left = 15
        Top = 345
        Width = 60
        Height = 15
        Caption = 'Thumbnail:'
      end
      object LabelStock: TLabel
        Left = 12
        Top = 242
        Width = 32
        Height = 15
        Caption = 'Stock:'
      end
      object LabelRating: TLabel
        Left = 12
        Top = 215
        Width = 37
        Height = 15
        Caption = 'Rating:'
      end
      object LabelPrice: TLabel
        Left = 12
        Top = 184
        Width = 29
        Height = 15
        Caption = 'Price:'
      end
      object Label1: TLabel
        Left = 10
        Top = 34
        Width = 13
        Height = 15
        Caption = 'Id:'
      end
      object LabelDiscountPercentage: TLabel
        Left = 12
        Top = 155
        Width = 112
        Height = 15
        Caption = 'Discount Percentage:'
      end
      object LabelDescription: TLabel
        Left = 12
        Top = 290
        Width = 63
        Height = 15
        Caption = 'Description:'
      end
      object LabelCategory: TLabel
        Left = 8
        Top = 121
        Width = 51
        Height = 15
        Caption = 'Category:'
      end
      object LabelBrand: TLabel
        Left = 8
        Top = 92
        Width = 34
        Height = 15
        Caption = 'Brand:'
      end
      object EditStock: TEdit
        Left = 146
        Top = 233
        Width = 420
        Height = 23
        TabOrder = 0
        Text = 'EditStock'
      end
      object Button3: TButton
        Left = 410
        Top = 371
        Width = 75
        Height = 25
        Action = actProductPrevious
        TabOrder = 1
      end
      object Button4: TButton
        Left = 491
        Top = 371
        Width = 75
        Height = 25
        Action = actProductNext
        TabOrder = 2
      end
      object EditTitle: TEdit
        Left = 146
        Top = 59
        Width = 420
        Height = 23
        TabOrder = 3
        Text = 'EditTitle'
      end
      object EditThumbnail: TEdit
        Left = 146
        Top = 342
        Width = 420
        Height = 23
        TabOrder = 4
        Text = 'EditThumbnail'
      end
      object EditRating: TEdit
        Left = 146
        Top = 204
        Width = 420
        Height = 23
        TabOrder = 5
        Text = 'EditRating'
      end
      object EditPrice: TEdit
        Left = 146
        Top = 175
        Width = 420
        Height = 23
        TabOrder = 6
        Text = 'EditPrice'
      end
      object EditId: TEdit
        Left = 146
        Top = 30
        Width = 50
        Height = 23
        TabOrder = 7
        Text = 'EditId'
      end
      object EditDiscountPercentage: TEdit
        Left = 146
        Top = 146
        Width = 420
        Height = 23
        TabOrder = 8
        Text = 'EditDiscountPercentage'
      end
      object EditDescription: TMemo
        Left = 146
        Top = 262
        Width = 420
        Height = 74
        Lines.Strings = (
          'EditDescription')
        TabOrder = 9
      end
      object EditCategory: TEdit
        Left = 146
        Top = 117
        Width = 420
        Height = 23
        TabOrder = 10
        Text = 'EditCategory'
      end
      object EditBrand: TEdit
        Left = 146
        Top = 88
        Width = 420
        Height = 23
        TabOrder = 11
        Text = 'EditBrand'
      end
    end
  end
  object ActionList1: TActionList
    Left = 80
    Top = 104
    object acUsertNext: TAction
      Caption = 'Next >>'
      OnExecute = acUsertNextExecute
    end
    object actUserPrevious: TAction
      Caption = 'Previous'
      OnExecute = actUserPreviousExecute
    end
    object actProductNext: TAction
      Caption = 'Next >>'
      OnExecute = actProductNextExecute
    end
    object actProductPrevious: TAction
      Caption = '<< Previous'
      OnExecute = actProductPreviousExecute
    end
  end
end
