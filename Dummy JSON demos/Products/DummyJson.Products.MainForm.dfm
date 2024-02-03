object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 293
  ClientWidth = 663
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object LabelBrand: TLabel
    Left = 8
    Top = 92
    Width = 34
    Height = 15
    Caption = 'Brand:'
  end
  object LabelCategory: TLabel
    Left = 8
    Top = 121
    Width = 51
    Height = 15
    Caption = 'Category:'
  end
  object LabelDescription: TLabel
    Left = 8
    Top = 167
    Width = 63
    Height = 15
    Caption = 'Description:'
  end
  object LabelDiscountPercentage: TLabel
    Left = 420
    Top = 34
    Width = 112
    Height = 15
    Caption = 'Discount Percentage:'
  end
  object LabelId: TLabel
    Left = 10
    Top = 34
    Width = 13
    Height = 15
    Caption = 'Id:'
  end
  object LabelPrice: TLabel
    Left = 420
    Top = 63
    Width = 29
    Height = 15
    Caption = 'Price:'
  end
  object LabelRating: TLabel
    Left = 420
    Top = 94
    Width = 37
    Height = 15
    Caption = 'Rating:'
  end
  object LabelStock: TLabel
    Left = 420
    Top = 121
    Width = 32
    Height = 15
    Caption = 'Stock:'
  end
  object LabelThumbnail: TLabel
    Left = 8
    Top = 230
    Width = 60
    Height = 15
    Caption = 'Thumbnail:'
  end
  object LabelTitle: TLabel
    Left = 8
    Top = 63
    Width = 25
    Height = 15
    Caption = 'Title:'
  end
  object EditBrand: TEdit
    Left = 98
    Top = 89
    Width = 295
    Height = 23
    TabOrder = 0
    Text = 'EditBrand'
  end
  object EditCategory: TEdit
    Left = 98
    Top = 118
    Width = 295
    Height = 23
    TabOrder = 1
    Text = 'EditCategory'
  end
  object EditDescription: TMemo
    Left = 98
    Top = 147
    Width = 295
    Height = 74
    Lines.Strings = (
      'EditDescription')
    TabOrder = 2
  end
  object EditDiscountPercentage: TEdit
    Left = 550
    Top = 31
    Width = 100
    Height = 23
    TabOrder = 3
    Text = 'EditDiscountPercentage'
  end
  object EditId: TEdit
    Left = 98
    Top = 31
    Width = 50
    Height = 23
    TabOrder = 4
    Text = 'EditId'
  end
  object EditPrice: TEdit
    Left = 550
    Top = 60
    Width = 100
    Height = 23
    TabOrder = 5
    Text = 'EditPrice'
  end
  object EditRating: TEdit
    Left = 550
    Top = 89
    Width = 100
    Height = 23
    TabOrder = 6
    Text = 'EditRating'
  end
  object EditStock: TEdit
    Left = 550
    Top = 118
    Width = 100
    Height = 23
    TabOrder = 7
    Text = 'EditStock'
  end
  object EditThumbnail: TEdit
    Left = 98
    Top = 227
    Width = 552
    Height = 23
    TabOrder = 8
    Text = 'EditThumbnail'
  end
  object EditTitle: TEdit
    Left = 98
    Top = 60
    Width = 295
    Height = 23
    TabOrder = 9
    Text = 'EditTitle'
  end
  object Button1: TButton
    Left = 575
    Top = 260
    Width = 75
    Height = 25
    Action = actNext
    TabOrder = 10
  end
  object Button2: TButton
    Left = 492
    Top = 260
    Width = 75
    Height = 25
    Action = actPrevious
    TabOrder = 11
  end
  object ActionList1: TActionList
    Left = 464
    Top = 136
    object actNext: TAction
      Caption = 'Next >>'
      OnExecute = actNextExecute
    end
    object actPrevious: TAction
      Caption = 'Previous'
      OnExecute = actPreviousExecute
    end
  end
end
