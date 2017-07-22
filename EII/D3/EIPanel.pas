////////////////////////////////////////////////////////////////
//                      EII Version 1.0a                      //
//            Copyright © 1998 by Markus Spoettl              //
//                    All rights reserved                     //
////////////////////////////////////////////////////////////////
//                                                            //
//   Please read the license agreement shipped with this      //
//   software. If you cannot agree to all terms of the        //
//   license agreement you MAY NOT use this software.         //
//                                                            //
//   *** CONTACT AUTHORS ***                                  //
//   If you have any questions regarding this software you    //
//   can contact                                              //
//     Markus Spoettl <markus@onlineloop.com>                 //
//   Please read the online help shipped with this software.  //
//                                                            //
//   *** WEB AND MAILING LIST ***                             //
//   Please visit the EII home page at                        //
//     http://www.onlineloop.com/markus/eii                   //
//   for latest news on EII. If you plan to use EII please    //
//   add yourself to the EII mailing list. You can find       //
//   details to how to subscribe to the mailing list at the   //
//   web pages.                                               //
//                                                            //
//   *** DISCLAIMER ***                                       //
//   EII IS PROVIDED "AS-IS". NO WARRANTIES OF ANY KIND,      //
//   EXPRESSED OR IMPLIED, ARE MADE AS TO IT OR ANY MEDIUM    //
//   IT MAY BE ON. NO REMEDY WILL BE PROVIDED FOR INDIRECT,   //
//   CONSEQUENTIAL, PUNITIVE OR INCIDENTAL DAMAGES ARISING    //
//   FROM IT, INCLUDING SUCH FROM NEGLIGENCE, STRICT          //
//   LIABILITY, OR BREACH OF WARRANTY OR CONTRACT, EVEN       //
//   AFTER NOTICE OF THE POSSIBILITY OF SUCH DAMAGES.         //
////////////////////////////////////////////////////////////////

//CE_Desc_Include(..\Help\EIPanel.Txt)

unit EIPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, buttons, menus;

const
    // Specifies the width of the title bar when aligned left or right
    // or the height when aligned top or bottom, also responsible for
    // the width and height of the buttons and for the buttons border
    EI_DRAGGINGPOINTFROMBORDER = 90;
    EI_DRAGGINGRECTFROMBORDER = 50;
    DEFAULT_TITLEBAR_WIDTH = 16;
    DEFAULT_TITLEBAR_BUTTONS_WIDTH = DEFAULT_TITLEBAR_WIDTH - 4;
    DEFAULT_TITLEBAR_BUTTONS_BORDER = (DEFAULT_TITLEBAR_WIDTH - DEFAULT_TITLEBAR_BUTTONS_WIDTH) div 2;

const
    CM_EI_PANELRELEASE = CM_BASE + 56;

type
    TTitleBarAlign = (talLeft, talRight, talTop, talBottom);
    TTitleBarButtonID = (tbbeX, tbbeLeft, tbbeRight, tbbeTop, tbbeBottom, tbbeFloat, tbbeMinMax, tbbeHelp);
    TTitleBarButtonIDs = set of TTitleBarButtonID;
    TTitleBarDragEvent = procedure (Sender: TObject; StartX: Integer; StartY: Integer) of Object;
    TTitleBarDrawEvent = procedure (Sender: TObject; Canvas: TCanvas; var Cancel:Boolean) of Object;

    TTitleButtonLayout = (tblGlyphLeft, tblGlyphRight);
    TEIPanel=class;
    // class for the title bar buttons
    // to be replaced with a own button control
    // in the released version (if neccessary)
    TTitleBarButton = class(TSpeedButton)
    private
        FCaption: String;
        FVertical: Boolean;
        procedure SetCaption(Value: String);
        procedure SetVertical(Value: Boolean);
    protected

      procedure Paint; override;
    public
      constructor Create(AOwner: TComponent); override; 
    published
        property Caption: String read FCaption write SetCaption;
        property Vertical: Boolean read FVertical write SetVertical;
    end;

    TTitleBarBtn=class(TObject)
    private
        FEIPanel:TEIPanel;

        FCaption: String;
        FGlyph: TBitmap;
        FVisible: BOOLEAN;
        FEnabled: Boolean;
        FButtonID: Integer;
        FWidth: Integer;
        FGroupIndex: Integer;
        FHint: String;
        FAllowAllUp: Boolean;
        FLayout: TTitleButtonLayout;
        FFont: TFont;

        procedure UpdateButton;
        procedure UpdateButtons;

        procedure SetCaption(Value: String);
        procedure SetGlyph(Value: TBitmap);
        procedure SetVisible(Value: BOOLEAN);
        procedure SetEnabled(Value: Boolean);
        procedure SetButtonID(Value: Integer);
        procedure SetWidth(Value: Integer);
        procedure SetGroupIndex(Value: Integer);
        procedure SetDown(Value: Boolean);
        function  GetDown: Boolean;
        procedure SetHint(Value: String);
        procedure SetAllowAllUp(Value: Boolean);
        procedure SetLayout(Value: TTitleButtonLayout); 
        function GetIndex: Integer;
        procedure FontChange(Sender: TObject); 
    public
        constructor Create(aEIPanel:TEIPanel);
        destructor Destroy; override;
        function Rect: TRect;
        procedure Popup(Menu: TPopupMenu);
        property Caption: String read FCaption write SetCaption;
        property Glyph: TBitmap read FGlyph write SetGlyph;
        property Visible: BOOLEAN read FVisible write SetVisible;
        property Enabled: Boolean read FEnabled write SetEnabled;
        property ButtonID: Integer read FButtonID write SetButtonID;
        property Width: Integer read FWidth write SetWidth;
        property GroupIndex: Integer read FGroupIndex write SetGroupIndex;
        property Down: Boolean read GetDown write SetDown;
        property Hint: String read FHint write SetHint;
        property Index: Integer read GetIndex;
        property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp;
        property Layout: TTitleButtonLayout read FLayout write SetLayout;
        property Font: TFont read FFont;
    published
    end;


    TTitleBar = class(TCustomPanel)
      private
        FDragStartX: Integer;
        FDragStartY: Integer;
        FDragging: BOOLEAN;

        FDragAllowed: BOOLEAN;
        FIcon: TIcon;

        FOnTitleBarStartDrag: TTitleBarDragEvent;
        FOnTitleBarEndDrag: TTitleBarDragEvent;
        FOnTitleBarDoDrag: TTitleBarDragEvent;
        FOnPaint: TTitleBarDrawEvent;
        FFilling: Boolean;

        procedure SetIcon(Value: TIcon);
        procedure DrawVertical(Canvas: TCanvas; Text: string);
        procedure SetFilling(Value: Boolean);
        procedure SetOnPaint(Value: TTitleBarDrawEvent);
      protected
        procedure Paint; override;
        procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
      public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
      published
        property Align;
        property Caption;
        property Color;
        property Font;
        property PopupMenu;
        property Visible;
        property ShowHint;
        property Hint;

        property DragAllowed: BOOLEAN read FDragAllowed write FDragAllowed;
        property OnTitleBarStartDrag: TTitleBarDragEvent read FOnTitleBarStartDrag write FOnTitleBarStartDrag;
        property OnTitleBarEndDrag: TTitleBarDragEvent read FOnTitleBarEndDrag write FOnTitleBarEndDrag;
        property OnTitleBarDoDrag: TTitleBarDragEvent read FOnTitleBarDoDrag write FOnTitleBarDoDrag;
        property OnPaint:TTitleBarDrawEvent read FOnPaint write SetOnPaint;
        property Icon: TIcon                read FIcon    write SetIcon;
        property Filling: Boolean read FFilling write SetFilling;
        end;

    TDragControl = class;
    TSplitterControl = class;

    TSplitterPanel = class(TCustomPanel)
      published
        property Align;
        property BevelInner;
        property BevelOuter;
        property BevelWidth;
        property BorderWidth;
        property BorderStyle;
        property Cursor;
        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
        end;

    TEIPanelButtonEvent = procedure (Sender: TObject; Button: TTitleBarButtonID; var Cancel: BOOLEAN) of Object;
    TEIPanelBtnEvent = procedure (Sender: TObject; ButtonID: Integer) of Object;
    TEIPanelCancelEvent  = procedure (Sender: TObject; var Cancel:Boolean) of Object;
    TEIPanelDragOffEvent = procedure (Sender: TObject; TargetScreenPoint: TPoint) of Object;
    TEIPanelDragToEvent  = procedure (Sender: TObject; Area:TAlign; var Cancel:Boolean) of Object;
    TEIPanelDragToFormEvent  = procedure (Sender: TObject; DestForm:TForm; Area:TAlign; TargetScreenPoint: TPoint) of Object;


    TEIPanel = class(TCustomPanel)
      private
        FDestroying: BOOLEAN;

        FTitleBar: TTitleBar;
        FNextButtonPos: Integer;

        FXButton: TTitleBarButton;
        FAlignLeftButton: TTitleBarButton;
        FAlignRightButton: TTitleBarButton;
        FAlignTopButton: TTitleBarButton;
        FAlignBottomButton: TTitleBarButton;
        FFloatButton: TTitleBarButton;
        FMinMaxButton: TTitleBarButton;
        FHelpButton: TTitleBarButton;

        FOnButtonClick: TEIPanelButtonEvent;
        FOnDragOffForm: TEIPanelDragOffEvent;

        FTitleBarAlign: TTitleBarAlign;
        FTitleBarButtonVisible: TTitleBarButtonIDs;
        FTitleBarButtonEnabled: TTitleBarButtonIDs;

        FSplitterPanel: TSplitterPanel;

        FDragControl: TDragControl;
        FSplitterControl: TSplitterControl;
        FMinimized: BOOLEAN;
        FRestoreMinimizedValue: Integer;
        FRestoreMinimizedSplitter: Integer;
        FTitleBarButtonFlat: BOOLEAN;
        FTitleBarBtnList:TList;
        FTitleBarButtonList:TList;
        FOnUserButtonClick: TEIPanelBtnEvent;
        FLockButtonUpdate:Boolean;
        FOnDragTo: TEIPanelDragToEvent;
        FOnTitleBarDblClick: TEIPanelCancelEvent;
        FOnAlign: TNotifyEvent;
        FOnDragToForm: TEIPanelDragToFormEvent;

        procedure CreateButtons;
        function GetIDFromButton(AButton: TTitleBarButton): TTitleBarButtonID;
        function GetTitleBarButtonFromID(ButtonID: TTitleBarButtonID): TTitleBarButton;

        function GetAlign: TAlign;
        procedure SetAlign(Value: TAlign);
        function GetSize: Integer;
        procedure SetSize(Value: Integer);

        function GetTitleBarVisible: BOOLEAN;
        procedure SetTitleBarVisible(Value: BOOLEAN);
        function GetTitleBarColor: TColor;
        procedure SetTitleBarColor(Value: TColor);
        function GetTitleBarFont: TFont;
        procedure SetTitleBarFont(Value: TFont);
        procedure SetTitleBarAlign(Value: TTitleBarAlign);
        procedure SetTitleBarButtonVisible(Value: TTitleBarButtonIDs);
        procedure SetTitleBarButtonEnabled(Value: TTitleBarButtonIDs);
        function GetTitleBarCaption: TCaption;
        procedure SetTitleBarCaption(Value: TCaption);
        function GetTitleBarHint:String;
        procedure SetTitleBarHint(Value:String);
        function GetTitleBarPopupMenu:TPopupMenu;
        procedure SetTitleBarPopupMenu(Value:TPopupMenu);
        function GetDragable: BOOLEAN;
        procedure SetDragable(Value: BOOLEAN);
        function GetSplitterAlign: TTitleBarAlign;
        procedure SetSplitterAlign(Value: TTitleBarAlign);
        function GetSplitterVisible: BOOLEAN;
        procedure SetSplitterVisible(Value: BOOLEAN);
        function GetSplitterColor:TColor;
        procedure SetSplitterColor(Value:TColor);
        function GetSplitterBevel:TPanelBevel;
        procedure SetSplitterBevel(Value:TPanelBevel);
        procedure SetMinimized(Value: BOOLEAN);
        {$IFDEF VER100}
        function GetButtonHint(ButtonID: TTitleBarButtonID): String;
        procedure SetButtonHint(ButtonID: TTitleBarButtonID; Value: String);
        {$ENDIF VER100}
        function GetTitleBarDrawEvent:TTitleBarDrawEvent;
        procedure SetTitleBarDrawEvent(Value:TTitleBarDrawEvent);
        function GetTitleBarIcon:TIcon;
        procedure SetTitleBarIcon(Value:TIcon);
        procedure UpdateMinMaxButton;
        procedure SetTitleBarButtonFlat(Value: BOOLEAN);
        function GetTitleBarBtn(Index:Integer):TTitleBarBtn;
        function GetIndexFromID(ButtonID:Integer):Integer;
        function GetTitleBarBtnFromID(ButtonId:Integer):TTitleBarBtn;
        function GetTitleBarButton(index: integer): TTitleBarButton;
        function GetTitleBarCanvas: TCanvas;
        function GetTitleBarFilling: Boolean;
        procedure SetTitleBarFilling(Value: Boolean);
      protected
        procedure CMEIPanelRelease(var msg: TMessage); message CM_EI_PANELRELEASE;

        procedure DoTitleBarDragStart(Sender: TObject; StartX, StartY: Integer); virtual;
        procedure DoTitleBarDragEnd(Sender: TObject; StartX, StartY: Integer); virtual;
        procedure DoTitleBarDragMove(Sender: TObject; StartX, StartY: Integer); virtual;

        procedure DoTitleBarDblClick(Sender: TObject); virtual;

        procedure DoSplitterMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
        procedure DoSplitterMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
        procedure DoSplitterMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual;

        procedure DoButtonClick(Sender: TObject); virtual;
        procedure SetupButton(Button: TTitleBarButton; ButtonVisible: BOOLEAN; ButtonEnabled: BOOLEAN; var NextButtonPos: Integer);
        procedure SetupTitleBar;
        procedure UpdateButton(Btn:TTitleBarBtn);
        procedure UpdateButtons(var NextButtonPos:Integer);
        procedure DoTitleBarButtonClick(Sender:TObject); virtual;
        property TitleBarButton[Index:Integer]: TTitleBarButton read GetTitleBarButton;

        procedure Resize; override;
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;

        procedure DoFree;
      public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        property Minimized: BOOLEAN read FMinimized write SetMinimized;
        procedure TitleBarRefresh;
        function IsTitleBarVertical: BOOLEAN;
        function AddButton:TTitleBarBtn;
        function InsertButton(Index:Integer):TTitleBarBtn;
        property Button[Index:Integer]:TTitleBarBtn read GetTitleBarBtn;
        property ButtonFromId[ButtonId:Integer]:TTitleBarBtn read GetTitleBarBtnFromID; default;
        property IndexFromId[ButtonID:Integer]:Integer read GetIndexFromID;

        function ButtonCount:Integer;
        procedure DeleteButton(Index:Integer);
        procedure ClearButtons;
        procedure ExchangeButtons(Index1,Index2:Integer);

        procedure LockButtonUpdate;
        procedure UnlockButtonUpdate;
        function TitleBarHeight: Integer; 
        function TitleBarClientRect: TRect;
        function IsVertical: Boolean;

        {$IFDEF VER110}
        // Moved the GetButtonHint and SetButtonHint methods
        // from private to public because BCB 3 doesn't
        // support the declaration of the ButtonHint property
        // (see below) for unknown reasons.
        function GetButtonHint(ButtonID: TTitleBarButtonID): String;
        procedure SetButtonHint(ButtonID: TTitleBarButtonID; Value: String);
        {$ENDIF VER110}

        function TitleBarVisibleWidth: Integer;
        property TitleBarCanvas: TCanvas read GetTitleBarCanvas;
      published
        property Color;
        property Caption;
        property BevelInner;
        property BevelOuter;

        //now the own properties
        property Align: TAlign read GetAlign write SetAlign;
        property Dragable: BOOLEAN read GetDragable write SetDragable;
        property Size: Integer read GetSize write SetSize;

        property OnButtonClick: TEIPanelButtonEvent read FOnButtonClick write FOnButtonClick;
        property OnDragOffForm: TEIPanelDragOffEvent read FOnDragOffForm write FOnDragOffForm;
        property OnDragTo: TEIPanelDragToEvent read FOnDragTo write FOnDragTo;
        property OnTitleBarPaint:TTitleBarDrawEvent read GetTitleBarDrawEvent write SetTitleBarDrawEvent;
        property OnTitleBarDblClick: TEIPanelCancelEvent read FOnTitleBarDblClick write FOnTitleBarDblClick;
        property OnUserButtonClick: TEIPanelBtnEvent read FOnUserButtonClick write FOnUserButtonClick;
        property OnAlign: TNotifyEvent read FOnAlign write FOnAlign;
        property OnDragToForm: TEIPanelDragToFormEvent read FOnDragToForm write FOnDragToForm;

        property TitleBarAlign: TTitleBarAlign read FTitleBarAlign write SetTitleBarAlign;
        property TitleBarVisible: BOOLEAN read GetTitleBarVisible write SetTitleBarVisible;
        property TitleBarButtonVisible: TTitleBarButtonIDs read FTitleBarButtonVisible write SetTitleBarButtonVisible;
        property TitleBarButtonEnabled: TTitleBarButtonIDs read FTitleBarButtonEnabled write SetTitleBarButtonEnabled;
        property TitleBarColor: TColor read GetTitleBarColor write SetTitleBarColor;
        property TitleBarFont: TFont read GetTitleBarFont write SetTitleBarFont;
        property TitleBarCaption: TCaption read GetTitleBarCaption write SetTitleBarCaption;

        property TitleBarHint: String          read GetTitleBarHint      write SetTitleBarHint;
        property TitleBarPopupMenu: TPopupMenu read GetTitleBarPopupMenu write SetTitleBarPopupMenu;
        property TitleBarIcon: TIcon           read GetTitleBarIcon      write SetTitleBarIcon;
        property TitleBarButtonFlat: BOOLEAN read FTitleBarButtonFlat write SetTitleBarButtonFlat;

        property SplitterAlign: TTitleBarAlign read GetSplitterAlign write SetSplitterAlign;
        property SplitterVisible: BOOLEAN read GetSplitterVisible write SetSplitterVisible;
        property SplitterColor:TColor read GetSplitterColor write SetSplitterColor;
        property SplitterBevel:TPanelBevel read GetSplitterBevel write SetSplitterBevel;
        {$IFDEF VER100}
        property ButtonHint[ButtonID: TTitleBarButtonID]: String read GetButtonHint write SetButtonHint;
        {$ENDIF VER100}
        property TitleBarFilling: Boolean read GetTitleBarFilling write SetTitleBarFilling;
        end;

    TDragControl = class(TObject)
      private
        FDragTarget: TControl;
        FDragging:Boolean;

        FControlCanvas: TControlCanvas;

        FDragRect: TRect;
        FDrawDragRect: TRect;
        FStartPoint: TPoint;
        FDragPoint: TPoint;

        procedure DrawDragRect;
      public
        procedure BeginDragging(ADragControl: TControl; CaptureControl: TControl; StartPoint: TPoint{; Form: TCustomForm});
        procedure ChangeDragging(DragPoint: TPoint);
        function EndDragging: TPoint;

        property StartPoint: TPoint read FStartPoint;
        property DragPoint: TPoint read FDragPoint;
        property DragRect: TRect read FDrawDragRect;
        end;

    TSplitterControl = class
      private
        FForm: TCustomForm;
        FSplitControl, FSizeTarget: TControl;
        FVertical: Boolean;
        FSplit: TPoint;
        FMin: Integer;
        FMax: Integer;

        function GetSizing: Boolean;
        procedure DrawSizingLine;
      public
        constructor Create(ASplitControl, ATargetControl: TControl);
        procedure BeginSizing(Vertical: BOOLEAN; DMIN: Integer; DMax: Integer; Form: TCustomForm);
        procedure ChangeSizing(X, Y: Integer);
        function EndSizing: Integer;
        property Sizing: Boolean read GetSizing;
        end;

function CToC(C1, C2: TControl; Point: TPoint): TPoint;

implementation

{$R edbuttons.res}

////////////////////////////////////////////////////////////////////////////////////////////////////
// TTitleBarButton
////////////////////////////////////////////////////////////////////////////////////////////////////

constructor TTitleBarButton.Create(AOwner: TComponent);
begin
  inherited;
  FCaption:='';
  FVertical:=false;
  Font.Name:='Arial';
  Font.Size:=7;
end;

procedure TTitleBarButton.Paint;
var LogFont: TLOGFONT;
    NewFont,OldFont: HFONT;
    TextLeft, TextTop: Integer;
    Rect: TRect;
begin
  inherited;
  Rect := GetClientRect;
  Canvas.Font:=Font;
  with Canvas do
  begin
    SetBkMode(Handle,TRANSPARENT);
    TextLeft:=0;
    TextTop:=0;
    if not Glyph.Empty then
    begin
      if FVertical then
      begin
        Spacing:=Height-Glyph.Height-16;
        if (Layout=blGlyphTop) then
        begin
          Rect.Top:=Rect.Top+Glyph.Height;
          TextTop:=Glyph.Height;
        end
        else if (Layout=blGlyphBottom) then
          Rect.Bottom:=Rect.Bottom-Glyph.Height;
      end
      else
      begin
        Spacing:=Width-Glyph.Width-6;
        if (Layout=blGlyphLeft) then
        begin
          Rect.Left:=Rect.Left+Glyph.Width;
          TextLeft:=Glyph.Width;
        end
        else if (Layout=blGlyphRight) then
          Rect.Right:=Rect.Right-Glyph.Width;
      end;
    end;
    if (FVertical) then
    begin
      GetObject(Canvas.Font.Handle, SizeOf(LogFont), @LogFont);
      LogFont.lfEscapement := 900;
      TextTop := TextTop+(Rect.Bottom-Rect.Top+TextWidth(FCaption)) div 2;
      TextLeft := TextLeft+(Rect.Right-Rect.Left-TextHeight(FCaption)) div 2+1;
      LogFont.lfOutPrecision := OUT_TT_ONLY_PRECIS;
      NewFont := CreateFontIndirect(LogFont);
      OldFont:=SelectObject(Canvas.Handle, NewFont);
      if (FState=bsDisabled) then
      begin
        SetTextColor(Handle,GetSysColor(COLOR_3DHILIGHT));
        Canvas.TextOut(TextLeft+1, TextTop+1, FCaption);
        SetTextColor(Handle,GetSysColor(COLOR_GRAYTEXT));
      end
      else if Down or (FState=bsDown) then
      begin
        TextLeft:=TextLeft+1;
        TextTop:=TextTop+1;
      end;
      Canvas.TextOut(TextLeft, TextTop, FCaption);
      SelectObject(Canvas.Handle,OldFont);
      DeleteObject(NewFont);
    end
    else
    begin
      TextTop := TextTop+(Rect.Bottom-Rect.Top-TextHeight(FCaption)) div 2;
      TextLeft := TextLeft+(Rect.Right-Rect.Left-TextWidth(FCaption)) div 2;
      if (FState=bsDisabled) then
      begin
        Font.Color:=GetSysColor(COLOR_3DHILIGHT);
        Canvas.TextOut(TextLeft+1, TextTop+1, FCaption);
        Font.Color:=GetSysColor(COLOR_GRAYTEXT);
      end
      else if Down or (FState=bsDown) then
      begin
        TextLeft:=TextLeft+1;
        TextTop:=TextTop+1;
      end;
      Canvas.TextOut(TextLeft, TextTop, FCaption);
    end;
  end;
end;

//Method_Marker(TTitleBarButton.SetCaption)

procedure TTitleBarButton.SetCaption(Value: String);
begin
    if (FCaption <> Value) then begin
        FCaption := Value;
        if FCaption='' then
        begin
          inherited Caption:='';
          Spacing:=0;
        end
        else
          inherited Caption:=' ';
        Refresh;
    end;
end;

//Method_Marker(TTitleBarButton.SetVertical)

procedure TTitleBarButton.SetVertical(Value: Boolean);
begin
    if (FVertical <> Value) then begin
        FVertical := Value;
        Refresh;
    end;
end;

{ClassMarker_Method(TTitleBarButton)}

////////////////////////////////////////////////////////////////////////////////////////////////////
// TTitleBarBtn
////////////////////////////////////////////////////////////////////////////////////////////////////

//Method_Marker(TTitleBarBtn.Create)

constructor TTitleBarBtn.Create(aEIPanel:TEIPanel);
begin
    inherited Create;
    FEIPanel:=aEIPanel;

    FButtonID:=0;
    FCaption:='';
//    FDown:=false;
    FEnabled:=true;
    FGlyph:=TBitmap.Create;
    FGlyph.TransparentMode:=tmAuto;
    FGroupIndex:=0;
    FHint:='';
    FVisible:=false;
    FWidth:=DEFAULT_TITLEBAR_BUTTONS_WIDTH;
    FFont:=TFont.Create;
    FFont.Name:='Arial';
    FFont.Size:=7;
    FFont.OnChange:=FontChange;
end;

//Method_Marker(TTitleBarBtn.Destroy)

destructor TTitleBarBtn.Destroy;
begin
    FGlyph.Free;
    inherited Destroy;
end;

procedure TTitleBarBtn.UpdateButton;
begin
  if FEIPanel<>nil then
    FEIPanel.UpdateButton(Self);
end;

procedure TTitleBarBtn.UpdateButtons;
begin
  if FEIPanel<>nil then
    FEIPanel.SetupTitleBar;
end;

//Method_Marker(TTitleBarBtn.SetCaption)

procedure TTitleBarBtn.SetCaption(Value: String);
begin
    if (FCaption <> Value) then begin
        FCaption := Value;
        UpdateButton;
    end;
end;

//Method_Marker(TTitleBarBtn.SetGlyph)

procedure TTitleBarBtn.SetGlyph(Value: TBitmap);
begin
    if (FGlyph <> Value) then begin
        FGlyph := Value;
        UpdateButton;
    end;
end;

//Method_Marker(TTitleBarBtn.SetVisible)

procedure TTitleBarBtn.SetVisible(Value: BOOLEAN);
begin
    if (FVisible <> Value) then begin
        FVisible := Value;
        UpdateButtons;
    end;
end;

//Method_Marker(TTitleBarBtn.SetEnabled)

procedure TTitleBarBtn.SetEnabled(Value: Boolean);
begin
    if (FEnabled <> Value) then begin
        FEnabled := Value;
        UpdateButton;
    end;
end;

//Method_Marker(TTitleBarBtn.SetButtonID)

procedure TTitleBarBtn.SetButtonID(Value: Integer);
begin
    if (FButtonID <> Value) then begin
        FButtonID := Value;
    end;
end;

//Method_Marker(TTitleBarBtn.SetWidth)

procedure TTitleBarBtn.SetWidth(Value: Integer);
begin
    if (FWidth <> Value) then begin
        FWidth := Value;
        UpdateButtons;
    end;
end;

//Method_Marker(TTitleBarBtn.SetGroupIndex)

procedure TTitleBarBtn.SetGroupIndex(Value: Integer);
begin
    if (FGroupIndex <> Value) then begin
        FGroupIndex := Value;
        UpdateButton;
    end;
end;

//Method_Marker(TTitleBarBtn.SetDown)

procedure TTitleBarBtn.SetDown(Value: Boolean);
begin
    if FEIPanel<>nil then
        FEIPanel.TitleBarButton[Index].Down:=Value;
end;

function TTitleBarBtn.GetDown: Boolean;
begin
  result:=false;
  if FEIPanel<>nil then
    result:=FEIPanel.TitleBarButton[Index].Down;
end;

//Method_Marker(TTitleBarBtn.SetHint)

procedure TTitleBarBtn.SetHint(Value: String);
begin
    if (FHint <> Value) then begin
        FHint := Value;
        UpdateButton;
    end;
end;

//Method_Marker(TTitleBarBtn.SetAllowAllUp)

procedure TTitleBarBtn.SetAllowAllUp(Value: Boolean);
begin
    if (FAllowAllUp <> Value) then begin
        FAllowAllUp := Value;
        UpdateButton;
    end;
end;

//Method_Marker(TTitleBarBtn.SetLayout)

procedure TTitleBarBtn.SetLayout(Value: TTitleButtonLayout);
begin
    if (FLayout <> Value) then begin
        FLayout := Value;
        UpdateButton;
    end;
end;

//Method_Marker(TTitleBarBtn.GetIndex)

function TTitleBarBtn.GetIndex: Integer;
begin
  result:=-1;
  if FEIPanel<>nil then
    result:=FEIPanel.IndexFromID[ButtonID];
end;

//Method_Marker(TTitleBarBtn.Rect)

function TTitleBarBtn.Rect: TRect;
var
  Button:TTitleBarButton;
begin
  if FEIPanel<>nil then
  begin
    Button:=FEIPanel.TitleBarButton[Index];
    result.TopLeft:=Button.Parent.ClientToScreen(Point(Button.Left,Button.Top));
    result.Bottom:=result.Top+Button.Height;
    result.Right:=result.Left+Button.Width;
  end;
end;

//Method_Marker(TTitleBarBtn.Popup)

procedure TTitleBarBtn.Popup(Menu: TPopupMenu);
begin
  if FEIPanel<>nil then
  begin
    if FEIPanel.IsTitleBarVertical then
      Menu.Popup(Rect.Right,Rect.Top)
    else
      Menu.Popup(Rect.Left,Rect.Bottom);
  end;
end;

//Method_Marker(TTitleBarBtn.FontChange)

procedure TTitleBarBtn.FontChange(Sender: TObject);
begin
  UpdateButton;
end;

{ClassMarker_Method(TTitleBarBtn)}

////////////////////////////////////////////////////////////////////////////////////////////////////
// TTitleBar
////////////////////////////////////////////////////////////////////////////////////////////////////

constructor TTitleBar.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    BevelInner := bvNone;
    BevelOuter := bvNone;

    Font.Name:='Arial';
    FIcon:=TIcon.Create;
    FFilling:=true;

    FDragStartX := -1;
    FDragStartY := -1;
    FDragging := FALSE;
    FDragAllowed := FALSE;
end;

destructor TTitleBar.Destroy;
begin
    FIcon.Free;
    inherited Destroy;
end;

procedure TTitleBar.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
  Refresh;
end;

procedure TTitleBar.DrawVertical(Canvas: TCanvas; Text: string);
var LogFont: TLOGFONT;
    NewFont, OldFont: HFONT;
    TextLeft, TextTop: Integer;
begin
    with Canvas do begin
        Canvas.Brush.Style := bsClear;

        GetObject(Canvas.Font.Handle, SizeOf(LogFont), @LogFont);
        LogFont.lfEscapement := 900;
        TextTop := Height - DEFAULT_TITLEBAR_BUTTONS_BORDER * 3;
        if not FIcon.Empty then
          TextTop:=TextTop-DEFAULT_TITLEBAR_WIDTH;
        TextLeft := (Width - Canvas.TextHeight(Text)) div 2;

        LogFont.lfOutPrecision := OUT_TT_ONLY_PRECIS;
        NewFont := CreateFontIndirect(LogFont);
        OldFont:=SelectObject(Canvas.Handle, NewFont);
        Canvas.TextOut(TextLeft, TextTop, Text);
        SelectObject(Canvas.Handle, OldFont);
        DeleteObject(NewFont);
        end;
end;

procedure TTitleBar.Paint;
var Rect: TRect;
    FontHeight: Integer;
    Cancel:Boolean;
begin
    Rect := GetClientRect;
    with Canvas do begin
        if FFilling then
        begin
          Brush.Color := Color;
          FillRect(Rect);
        end;

        Cancel:=false;
        if assigned(FOnPaint) then
          FOnPaint(Self,Canvas,Cancel);
        if Cancel then
          exit;
          
        Brush.Style := bsClear;
        SetBkMode(Canvas.Handle,TRANSPARENT);
        if not FIcon.Empty then
        begin
          if (Align=alTop) or (Align=alBottom) then
            DrawIconEx(Handle,0,0,
              FIcon.Handle,DEFAULT_TITLEBAR_WIDTH,DEFAULT_TITLEBAR_WIDTH,0,0,DI_Normal)
          else
            DrawIconEx(Handle,0,Self.Height-DEFAULT_TITLEBAR_WIDTH,
              FIcon.Handle,DEFAULT_TITLEBAR_WIDTH,DEFAULT_TITLEBAR_WIDTH,0,0,DI_Normal);
        end;

        Font := Self.Font;

        // If the alignment of the titlepanel is left or top
        // we draw the title (caption) vertical.
        if ((Align = alLeft) or (Align = alRight)) then begin
            DrawVertical(Canvas, Caption);
            end
        else begin
            // else: we draw as the panel does
            FontHeight := TextHeight('W');
            with Rect do begin
                Left := DEFAULT_TITLEBAR_BUTTONS_BORDER * 3;
                if not FIcon.Empty then
                  Left:=Left+DEFAULT_TITLEBAR_WIDTH;
                Top := ((Bottom + Top) - FontHeight) div 2;
                Bottom := Top + FontHeight;
                end;
            DrawText(Handle, PChar(Caption), -1, Rect, (DT_EXPANDTABS or DT_VCENTER) or DT_LEFT);
            end;
          end;
end;

procedure TTitleBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    inherited MouseDown(Button, Shift, X, Y);

    if (FDragAllowed) then begin
        FDragStartX := X;
        FDragStartY := Y;
        end;
end;

procedure TTitleBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    inherited MouseUp(Button, Shift, X, Y);

    if ((FDragAllowed) and (FDragging)) then begin
        if (Assigned(FOnTitleBarEndDrag)) then
            FOnTitleBarEndDrag(self, X, Y);
        end;

    FDragging := FALSE;
    FDragStartX := -1;
    FDragStartY := -1;
end;

procedure TTitleBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
    inherited MouseMove(Shift, X, Y);

    if (FDragAllowed) then begin
        if ((FDragStartX <> -1) and (FDragStartY <> -1)) then begin
            if (not FDragging)then begin
                if ((Abs(X-FDragStartX) > 5) or (Abs(Y-FDragStartY) > 5)) then begin
                    if (Assigned(FOnTitleBarStartDrag)) then begin
                        FOnTitleBarStartDrag(Self, X, Y);
                        FDragging := TRUE;
                        end;
                    end;
                end
            else begin
                if (Assigned(FOnTitleBarDoDrag)) then begin
                    FOnTitleBarDoDrag(Self, X, Y);
                    end;
                end;
            end;
        end;

end;

//Method_Marker(TTitleBar.SetFilling)

procedure TTitleBar.SetFilling(Value: Boolean);
begin
  if (FFilling<>Value) and (assigned(FOnPaint) or (Value=true)) then
  begin
    FFilling := Value;
    if FFilling then
      Refresh;
  end;
end;

procedure TTitleBar.SetOnPaint(Value: TTitleBarDrawEvent);
begin
  FOnPaint:=Value;
  if not assigned(Value) then
    Filling:=false;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TEIPanel
////////////////////////////////////////////////////////////////////////////////////////////////////

constructor TEIPanel.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    BevelOuter := bvRaised;
    BevelInner := bvNone;

    FDestroying := FALSE;

    Caption := '';

    FTitleBar := TTitleBar.Create(Self);
    FTitleBar.Parent := Self;
    FTitleBar.OnTitleBarStartDrag := DoTitleBarDragStart;
    FTitleBar.OnTitleBarEndDrag := DoTitleBarDragEnd;
    FTitleBar.OnTitleBarDoDrag := DoTitleBarDragMove;
    FTitleBar.OnDblClick := DoTitleBarDblClick;

    FTitleBar.DragAllowed := TRUE;
    FTitleBar.ShowHint := TRUE;

    CreateButtons;

    FOnButtonClick := nil;

    FTitleBarAlign := talLeft;
    FTitleBar.Color := clActiveCaption;
    FTitleBar.Font.Color := clCaptionText;
    FTitleBar.Font.Style := [fsBold];
    FTitleBar.BevelOuter := bvNone;
    FTitleBar.BevelInner := bvNone;
    TitleBarCaption := 'Title';

    //by default all buttons are visible and enabled
    FTitleBarButtonVisible := [tbbeX, tbbeLeft, tbbeRight, tbbeTop, tbbeBottom];
    FTitleBarButtonEnabled := [tbbeX, tbbeLeft, tbbeRight, tbbeTop, tbbeBottom];

    FSplitterPanel := TSplitterPanel.Create(self);
    FSplitterPanel.Parent := self;
    FSplitterPanel.OnMouseDown := DoSplitterMouseDown;
    FSplitterPanel.OnMouseUp := DoSplitterMouseUp;
    FSplitterPanel.OnMouseMove := DoSplitterMouseMove;
    FSplitterPanel.Caption := '';
    SetSplitterAlign(talRight);

    FDragControl := TDragControl.Create;
    FSplitterControl := TSplitterControl.Create(FSplitterPanel, Self);

    FRestoreMinimizedValue := -1;
    FRestoreMinimizedSplitter := -1;

    FTitleBarBtnList:=TList.Create;
    FTitleBarButtonList:=TList.Create;
    SetupTitleBar;
end;

destructor TEIPanel.Destroy;
begin
    // To ignore the WM_SIZING message which occures
    // when the Panel is destroyed
    TitleBarButtonFlat:=false;

    FDestroying := TRUE;

    FDragControl.Free;

    FXButton.Free;
    FAlignLeftButton.Free;
    FAlignRightButton.Free;
    FAlignTopButton.Free;
    FAlignBottomButton.Free;
    FFloatButton.Free;
    FMinMaxButton.Free;
    FHelpButton.Free;

    FTitleBarBtnList.Free;
    FTitleBarButtonList.Free;
    FTitleBar.Free;

    inherited Destroy;
end;

procedure TEIPanel.TitleBarRefresh;
begin
  FTitleBar.Refresh;
end;

procedure TEIPanel.CMEIPanelRelease(var msg: TMessage);
begin
    if ((Msg.lParam = CM_EI_PANELRELEASE) and (Msg.wParam = CM_EI_PANELRELEASE)) then begin
        Free;
        end
    else begin
        inherited;
        end;
end;

procedure TEIPanel.DoFree;
begin
    PostMessage(Handle, CM_EI_PANELRELEASE, CM_EI_PANELRELEASE, CM_EI_PANELRELEASE);
end;

procedure TEIPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
    if (Operation = opRemove) then begin
        if (AComponent = FXButton) then
           FXButton := nil
        else if (AComponent = FAlignLeftButton) then
           FAlignLeftButton := nil
        else if (AComponent = FAlignRightButton) then
           FAlignRightButton := nil
        else if (AComponent = FAlignTopButton) then
           FAlignTopButton := nil
        else if (AComponent = FAlignBottomButton) then 
           FAlignBottomButton := nil
        else if (AComponent = FFloatButton) then
           FFloatButton := nil
        else if (AComponent = FMinMaxButton) then
           FMinMaxButton := nil
        else if (AComponent = FHelpButton) then
           FHelpButton := nil
        else if (AComponent = FTitleBar) then
           FTitleBar := nil;
        end;
        
    inherited Notification(AComponent, Operation);
end;

procedure TEIPanel.CreateButtons;
begin
    FXButton := TTitleBarButton.Create(FTitleBar);
    FXButton.Parent := FTitleBar;
    FXButton.Width := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
    FXButton.Height := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
    FAlignLeftButton := TTitleBarButton.Create(FTitleBar);
    FAlignLeftButton.Parent := FTitleBar;
    FAlignLeftButton.Width := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
    FAlignLeftButton.Height := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
    FAlignRightButton := TTitleBarButton.Create(FTitleBar);
    FAlignRightButton.Parent := FTitleBar;
    FAlignRightButton.Width := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
    FAlignRightButton.Height := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
    FAlignTopButton := TTitleBarButton.Create(FTitleBar);
    FAlignTopButton.Parent := FTitleBar;
    FAlignTopButton.Width := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
    FAlignTopButton.Height := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
    FAlignBottomButton := TTitleBarButton.Create(FTitleBar);
    FAlignBottomButton.Parent := FTitleBar;
    FAlignBottomButton.Width := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
    FAlignBottomButton.Height := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
    FFloatButton := TTitleBarButton.Create(FTitleBar);
    FFloatButton.Parent := FTitleBar;
    FFloatButton.Width := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
    FFloatButton.Height := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
    FMinMaxButton := TTitleBarButton.Create(FTitleBar);
    FMinMaxButton.Parent := FTitleBar;
    FMinMaxButton.Width := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
    FMinMaxButton.Height := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
    FHelpButton := TTitleBarButton.Create(FTitleBar);
    FHelpButton.Parent := FTitleBar;
    FHelpButton.Width := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
    FHelpButton.Height := DEFAULT_TITLEBAR_BUTTONS_WIDTH;

    FXButton.Glyph.LoadFromResourceName(HInstance, 'EDITOR_INTEGRATION_PANEL_BTN_X');
    FAlignLeftButton.Glyph.LoadFromResourceName(HInstance, 'EDITOR_INTEGRATION_PANEL_BTN_LEFT');
    FAlignRightButton.Glyph.LoadFromResourceName(HInstance, 'EDITOR_INTEGRATION_PANEL_BTN_RIGHT');
    FAlignTopButton.Glyph.LoadFromResourceName(HInstance, 'EDITOR_INTEGRATION_PANEL_BTN_TOP');
    FAlignBottomButton.Glyph.LoadFromResourceName(HInstance, 'EDITOR_INTEGRATION_PANEL_BTN_BOTTOM');
    FFloatButton.Glyph.LoadFromResourceName(HInstance, 'EDITOR_INTEGRATION_PANEL_BTN_FLOAT');
    FMinMaxButton.Glyph.LoadFromResourceName(HInstance, 'EDITOR_INTEGRATION_PANEL_BTN_MINIMIZE');
    FHelpButton.Glyph.LoadFromResourceName(HInstance, 'EDITOR_INTEGRATION_PANEL_BTN_HELP');

    FXButton.Layout := blGlyphRight;
    FAlignLeftButton.Layout := blGlyphRight;
    FAlignRightButton.Layout := blGlyphRight;
    FAlignTopButton.Layout := blGlyphRight;
    FAlignBottomButton.Layout := blGlyphRight;
    FFloatButton.Layout := blGlyphRight;
    FMinMaxButton.Layout := blGlyphRight;
    FHelpButton.Layout := blGlyphRight;

    FXButton.OnClick := DoButtonClick;
    FAlignLeftButton.OnClick := DoButtonClick;
    FAlignRightButton.OnClick := DoButtonClick;
    FAlignTopButton.OnClick := DoButtonClick;
    FAlignBottomButton.OnClick := DoButtonClick;
    FFloatButton.OnClick := DoButtonClick;
    FMinMaxButton.OnClick := DoButtonClick;
    FHelpButton.OnClick := DoButtonClick;
end;

procedure TEIPanel.SetupButton(Button: TTitleBarButton; ButtonVisible: BOOLEAN; ButtonEnabled: BOOLEAN; var NextButtonPos: Integer);
begin
    if (FDestroying) then exit;

    if (Button.Visible <> ButtonVisible) then
        Button.Visible := ButtonVisible;

    if (ButtonVisible) then begin
        if (Button.Enabled <> ButtonEnabled) then
            Button.Enabled := ButtonEnabled;

    if (Button.Flat<>FTitleBarButtonFlat) then
      Button.Flat:=FTitleBarButtonFlat;

        case FTitleBarAlign of
            talLeft, talRight: begin
                if (Button.Left <> DEFAULT_TITLEBAR_BUTTONS_BORDER) then
                    Button.Left := DEFAULT_TITLEBAR_BUTTONS_BORDER;
                if (Button.Top <> NextButtonPos{-Button.Height+DEFAULT_TITLEBAR_BUTTONS_WIDTH}) then
                    Button.Top := NextButtonPos{-Button.Height+DEFAULT_TITLEBAR_BUTTONS_WIDTH};

                NextButtonPos := NextButtonPos + Button.Height;
                end; {talLeft, talRight}
            talTop, talBottom: begin
                if (Button.Left <> NextButtonPos-Button.Width+DEFAULT_TITLEBAR_BUTTONS_WIDTH) then
                    Button.Left := NextButtonPos-Button.Width+DEFAULT_TITLEBAR_BUTTONS_WIDTH;
                if (Button.Top <> DEFAULT_TITLEBAR_BUTTONS_BORDER) then
                    Button.Top := DEFAULT_TITLEBAR_BUTTONS_BORDER;

                NextButtonPos := NextButtonPos - Button.Width;
                end; {talTop, talBottom}
            end; {case}
        end;
end;


procedure TEIPanel.SetupTitleBar;
var NextButtonPos: Integer;
begin
    if (FDestroying) or (FLockButtonUpdate) then exit;

    if ((FTitleBar <> nil) and FTitleBar.Visible) then begin
        case FTitleBarAlign of
            talLeft, talRight: begin
                FTitleBar.Width := DEFAULT_TITLEBAR_WIDTH;
                if (FTitleBarAlign = talLeft) then
                    FTitleBar.Align := alLeft
                else if (FTitleBarAlign = talRight) then
                    FTitleBar.Align := alRight;

                NextButtonPos := DEFAULT_TITLEBAR_BUTTONS_BORDER;
                end; {talLeft, talRight}
            talTop, talBottom: begin
                FTitleBar.Width := DEFAULT_TITLEBAR_WIDTH;
                if (FTitleBarAlign = talTop) then
                    FTitleBar.Align := alTop
                else if (FTitleBarAlign = talBottom) then
                    FTitleBar.Align := alBottom;

                NextButtonPos := FTitleBar.Width - DEFAULT_TITLEBAR_BUTTONS_BORDER - DEFAULT_TITLEBAR_BUTTONS_WIDTH;
                end; {talTop, talBottom}
        end; {case}

        if (FXButton <> nil) then
            SetupButton(FXButton, tbbeX in FTitleBarButtonVisible, tbbeX in FTitleBarButtonEnabled, NextButtonPos);
        if (FHelpButton <> nil) then
            SetupButton(FHelpButton, tbbeHelp in FTitleBarButtonVisible, tbbeHelp in FTitleBarButtonEnabled, NextButtonPos);
        if (FMinMaxButton <> nil) then
        begin
            SetupButton(FMinMaxButton, tbbeMinMax in FTitleBarButtonVisible, false{tbbeMinMax in FTitleBarButtonEnabled}, NextButtonPos);
            UpdateMinMaxButton;
        end;
        if (FAlignLeftButton <> nil) then
            SetupButton(FAlignLeftButton, tbbeLeft in FTitleBarButtonVisible, tbbeLeft in FTitleBarButtonEnabled, NextButtonPos);
        if (FAlignRightButton <> nil) then
            SetupButton(FAlignRightButton, tbbeRight in FTitleBarButtonVisible, tbbeRight in FTitleBarButtonEnabled, NextButtonPos);
        if (FAlignTopButton <> nil) then
            SetupButton(FAlignTopButton, tbbeTop in FTitleBarButtonVisible, tbbeTop in FTitleBarButtonEnabled, NextButtonPos);
        if (FAlignBottomButton <> nil) then
            SetupButton(FAlignBottomButton, tbbeBottom in FTitleBarButtonVisible, tbbeBottom in FTitleBarButtonEnabled, NextButtonPos);
        if (FFloatButton <> nil) then
            SetupButton(FFloatButton, tbbeFloat in FTitleBarButtonVisible, tbbeFloat in FTitleBarButtonEnabled, NextButtonPos);
        UpdateButtons(NextButtonPos);
        FNextButtonPos:=NextButtonPos;
    end;
end;

procedure TEIPanel.UpdateButton(Btn:TTitleBarBtn);
var
  Button:TTitleBarButton;
begin
  Button:=TTitleBarButton(FTitleBarButtonList[Btn.Index]);
  if Button<>nil then
  begin
    Button.Caption:=Btn.Caption;
    Button.Glyph:=Btn.Glyph;
    Button.Hint:=Btn.Hint;
    Button.AllowAllUp:=Btn.AllowAllUp;
    Button.Font.Assign(Btn.Font);

    Button.GroupIndex:=Btn.GroupIndex;
    Button.Enabled:=Btn.Enabled;
    if (Btn.Layout=tblGlyphLeft) then
    begin
      if Button.Vertical then
        Button.Layout:=blGlyphBottom
      else
        Button.Layout:=blGlyphLeft;
    end
    else
    begin
      if Button.Vertical then
        Button.Layout:=blGlyphTop
      else
        Button.Layout:=blGlyphRight;
    end;
  end;
end;

procedure TEIPanel.UpdateButtons(var NextButtonPos:Integer);
var
  i:Integer;
  Button:TTitleBarButton;
  Btn:TTitleBarBtn;
begin
  for i:=0 to ButtonCount-1 do
  begin
    Button:=TTitleBarButton(FTitleBarButtonList[i]);
    Btn:=TTitleBarBtn(FTitleBarBtnList[i]);
    if (Button<>nil) then
    begin
      Button.Vertical:=IsTitleBarVertical;
      UpdateButton(Btn);
      if Button.Vertical then
      begin
        Button.Height:=Btn.Width;
        Button.Width:=DEFAULT_TITLEBAR_BUTTONS_WIDTH
      end
      else
      begin
        Button.Width:=Btn.Width;
        Button.Height:=DEFAULT_TITLEBAR_BUTTONS_WIDTH;
      end;
      SetupButton(Button,Btn.Visible,Btn.Enabled,NextButtonPos);
    end;
  end;
end;

procedure TEIPanel.DoTitleBarButtonClick(Sender:TObject);
var
  Btn:TTitleBarBtn;
begin
  Btn:=TTitleBarBtn(FTitleBarBtnList[FTitleBarButtonList.IndexOf(Sender)]);

  if assigned(FOnUserButtonClick) then
    FOnUserButtonClick(Self,Btn.ButtonID);
end;

procedure TEIPanel.DoTitleBarDragStart(Sender: TObject; StartX, StartY: Integer);
begin
    if ((Sender <> nil) and (Sender is TTitleBar)) then begin
        FDragControl.BeginDragging(Self, Sender as TTitleBar, CToC(Self, Sender as TTitleBar, Point(StartX,StartY)));
        end;
end;

function PointInRect(Point: TPoint; Rect: TRect): BOOLEAN;
begin
    result :=   (Point.X >= Rect.Left) AND (Point.X <= Rect.Right) AND
                (Point.Y >= Rect.Top) AND (Point.Y <= Rect.Bottom);
end;

procedure TEIPanel.DoTitleBarDragEnd(Sender: TObject; StartX, StartY: Integer);
var form: TCustomForm;
    dest: TForm;
    formRect: TRect;
    R:Trect;
    Cancel:Boolean;
    Area:TAlign;
    PointTL,PointBR:TPoint;
    Wnd:HWnd;
    i:Integer;
begin
  if ((Sender <> nil) and (Sender is TTitleBar)) then
  begin
    FDragControl.EndDragging;
    form := GetParentForm(Self);
    if (form <> nil) then
    begin
      formRect.TopLeft := Point(form.Left, form.Top);
      formRect.BottomRight := Point(form.Left+form.Width, form.Top+form.Height);
      if (not PointInRect(FDragControl.DragPoint, formRect)) then
      begin
        GetCursorPos(PointTL);
        Wnd:=WindowFromPoint(PointTL);
        dest:=nil;
        // check for a destination form...
        while (dest=nil) and (Wnd<>0) do
        begin
          for i:=0 to Screen.FormCount-1 do
          if (Screen.Forms[i].Handle=Wnd) and
             (not (Screen.Forms[i].Parent is TEIPanel)) then
          begin
            dest:=Screen.Forms[i];
            break;
          end;
          Wnd:=GetParent(Wnd);
        end;
        // form found!
        if (dest<>nil) and assigned(FOnDragToForm) then
        begin
          Area:=alNone;
          PointTL:=dest.ClientToScreen(Point(0,0));
          PointBR:=dest.ClientToScreen(Point(dest.ClientWidth,
                                             dest.ClientHeight));
          with FDragControl do
          begin
            // Drag to Left...
            if ((DragRect.Top   +EI_DRAGGINGRECTFROMBORDER)>PointTL.Y+Top) and
               ((DragRect.Top   -EI_DRAGGINGRECTFROMBORDER)<PointTL.Y+Top) and
               ((DragRect.Left  +EI_DRAGGINGRECTFROMBORDER)>PointTL.X) and
               ((DragRect.Left  -EI_DRAGGINGRECTFROMBORDER)<PointTL.X) and
               ((DragRect.Bottom+EI_DRAGGINGRECTFROMBORDER)>PointTL.Y+Top+Height) and
               ((DragRect.Bottom-EI_DRAGGINGRECTFROMBORDER)<PointTL.Y+Top+Height) then
              Area:=alLeft;
            // Drag to Right...
            if ((DragRect.Top   +EI_DRAGGINGRECTFROMBORDER)>PointTL.Y+Top) and
               ((DragRect.Top   -EI_DRAGGINGRECTFROMBORDER)<PointTL.Y+Top) and
               ((DragRect.Right +EI_DRAGGINGRECTFROMBORDER)>PointBR.X) and
               ((DragRect.Right -EI_DRAGGINGRECTFROMBORDER)<PointBR.X) and
               ((DragRect.Bottom+EI_DRAGGINGRECTFROMBORDER)>PointTL.Y+Top+Height) and
               ((DragRect.Bottom-EI_DRAGGINGRECTFROMBORDER)<PointTL.Y+Top+Height) then
              Area:=alRight;
            // Drag to Bottom...
            if ((DragRect.Right +EI_DRAGGINGRECTFROMBORDER)>PointBR.X) and
               ((DragRect.Right -EI_DRAGGINGRECTFROMBORDER)<PointBR.X) and
               ((DragRect.Left  +EI_DRAGGINGRECTFROMBORDER)>PointTL.X) and
               ((DragRect.Left  -EI_DRAGGINGRECTFROMBORDER)<PointTL.X) and
               ((DragRect.Bottom+EI_DRAGGINGRECTFROMBORDER)>PointBR.Y) and
               ((DragRect.Bottom-EI_DRAGGINGRECTFROMBORDER)<PointBR.Y) then
              Area:=alBottom;
            // Drag to Top...
            if ((DragRect.Right +EI_DRAGGINGRECTFROMBORDER)>PointBR.X) and
               ((DragRect.Right -EI_DRAGGINGRECTFROMBORDER)<PointBR.X) and
               ((DragRect.Left  +EI_DRAGGINGRECTFROMBORDER)>PointTL.X) and
               ((DragRect.Left  -EI_DRAGGINGRECTFROMBORDER)<PointTL.X) and
               ((DragRect.Top   +EI_DRAGGINGRECTFROMBORDER)>PointTL.Y) and
               ((DragRect.Top   -EI_DRAGGINGRECTFROMBORDER)<PointTL.Y) then
              Area:=alTop;
          end;
          if Area=alNone then
          begin
            R.Left:=-PointTL.x+FDragControl.DragPoint.x;
            R.Right:=PointBR.x-FDragControl.DragPoint.x;
            R.Top:=-PointTL.y+FDragControl.FDragPoint.y;
            R.Bottom:=PointBR.y-FDragControl.DragPoint.y;
            if R.Top<EI_DRAGGINGPOINTFROMBORDER then
              Area:=alTop;
            if R.Bottom<EI_DRAGGINGPOINTFROMBORDER then
              Area:=alBottom;
            if R.Left<EI_DRAGGINGPOINTFROMBORDER then
              Area:=alLeft;
            if R.Right<EI_DRAGGINGPOINTFROMBORDER then
              Area:=alRight;
          end;
          if (Area=alNone) then
            Area:=Align;
          if assigned(FOnDragToForm) then
            FOnDragToForm(Self, dest, Area, FDragControl.DragRect.TopLeft);
        end
        else if (Assigned(FOnDragOffForm)) then
          // form not found!
          FOnDragOffForm(self, FDragControl.DragRect.TopLeft);
      end
      else
      begin
        // normal dragging...
        Area:=alNone;
        PointTL:=form.ClientToScreen(Point(0,0));
        PointBR:=form.ClientToScreen(Point(form.ClientWidth,
                                           form.ClientHeight));
        with FDragControl do
        begin
          // Drag to Left...
          if ((DragRect.Top   +EI_DRAGGINGRECTFROMBORDER)>PointTL.Y+Top) and
             ((DragRect.Top   -EI_DRAGGINGRECTFROMBORDER)<PointTL.Y+Top) and
             ((DragRect.Left  +EI_DRAGGINGRECTFROMBORDER)>PointTL.X) and
             ((DragRect.Left  -EI_DRAGGINGRECTFROMBORDER)<PointTL.X) and
             ((DragRect.Bottom+EI_DRAGGINGRECTFROMBORDER)>PointTL.Y+Top+Height) and
             ((DragRect.Bottom-EI_DRAGGINGRECTFROMBORDER)<PointTL.Y+Top+Height) then
            Area:=alLeft;
          // Drag to Right...
          if ((DragRect.Top   +EI_DRAGGINGRECTFROMBORDER)>PointTL.Y+Top) and
             ((DragRect.Top   -EI_DRAGGINGRECTFROMBORDER)<PointTL.Y+Top) and
             ((DragRect.Right +EI_DRAGGINGRECTFROMBORDER)>PointBR.X) and
             ((DragRect.Right -EI_DRAGGINGRECTFROMBORDER)<PointBR.X) and
             ((DragRect.Bottom+EI_DRAGGINGRECTFROMBORDER)>PointTL.Y+Top+Height) and
             ((DragRect.Bottom-EI_DRAGGINGRECTFROMBORDER)<PointTL.Y+Top+Height) then
            Area:=alRight;
          // Drag to Bottom...
          if ((DragRect.Right +EI_DRAGGINGRECTFROMBORDER)>PointBR.X) and
             ((DragRect.Right -EI_DRAGGINGRECTFROMBORDER)<PointBR.X) and
             ((DragRect.Left  +EI_DRAGGINGRECTFROMBORDER)>PointTL.X) and
             ((DragRect.Left  -EI_DRAGGINGRECTFROMBORDER)<PointTL.X) and
             ((DragRect.Bottom+EI_DRAGGINGRECTFROMBORDER)>PointBR.Y) and
             ((DragRect.Bottom-EI_DRAGGINGRECTFROMBORDER)<PointBR.Y) then
            Area:=alBottom;
          // Drag to Top...
          if ((DragRect.Right +EI_DRAGGINGRECTFROMBORDER)>PointBR.X) and
             ((DragRect.Right -EI_DRAGGINGRECTFROMBORDER)<PointBR.X) and
             ((DragRect.Left  +EI_DRAGGINGRECTFROMBORDER)>PointTL.X) and
             ((DragRect.Left  -EI_DRAGGINGRECTFROMBORDER)<PointTL.X) and
             ((DragRect.Top   +EI_DRAGGINGRECTFROMBORDER)>PointTL.Y) and
             ((DragRect.Top   -EI_DRAGGINGRECTFROMBORDER)<PointTL.Y) then
            Area:=alTop;
        end;
        if Area=alNone then
        begin
          R.Left:=-PointTL.x+FDragControl.DragPoint.x;
          R.Right:=PointBR.x-FDragControl.DragPoint.x;
          R.Top:=-PointTL.y+FDragControl.FDragPoint.y;
          R.Bottom:=PointBR.y-FDragControl.DragPoint.y;
          if R.Top<EI_DRAGGINGPOINTFROMBORDER then
            Area:=alTop;
          if R.Bottom<EI_DRAGGINGPOINTFROMBORDER then
            Area:=alBottom;
          if R.Left<EI_DRAGGINGPOINTFROMBORDER then
            Area:=alLeft;
          if R.Right<EI_DRAGGINGPOINTFROMBORDER then
            Area:=alRight;
        end;
        if (Area<>Align) and (Area<>alNone) then
        begin
          Cancel:=false;
          if assigned(FOnDragTo) then
            FOnDragTo(Self,Area,Cancel);
          if not Cancel then
            Align:=Area;
        end;
      end;
    end;
  end;
end;

procedure TEIPanel.DoTitleBarDragMove(Sender: TObject; StartX, StartY: Integer);
begin
    if ((Sender <> nil) and (Sender is TTitleBar)) then begin
        FDragControl.ChangeDragging(CToC(Self, Sender as TTitleBar, Point(StartX,StartY)));
        end;
end;

procedure TEIPanel.DoTitleBarDblClick(Sender: TObject);
var
  Cancel:Boolean;
begin
    if (((Align = alLeft) or (Align = alRight)) and
            ((TitleBarAlign = talLeft) or (TitleBarAlign = talRight))) or
            (((Align = alTop) or (Align = alBottom)) and
            ((TitleBarAlign = talTop) or (TitleBarAlign = talBottom))) then begin
        FLockButtonUpdate:=false;
        FDragControl.EndDragging;
        Cancel:=false;

        if (Assigned(FOnTitleBarDblClick)) then begin
            FOnTitleBarDblClick(self, Cancel);
            end;
        if (not Cancel) then begin
            Minimized := not Minimized;
            end;
        end;
end;

procedure TEIPanel.DoSplitterMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var form: TCustomForm;
    vertical: BOOLEAN;
    dmin, dmax: Integer;
    borders: integer;
    titlewidth: integer;
begin
    form := GetParentForm(Self);
    if (form <> nil) then begin
        if (Button = mbLeft) and (Shift = [ssLeft]) then begin
            borders := 0;
            if (BevelInner <> bvNone) then
               borders := 2 * BevelWidth;
            if (BevelOuter <> bvNone) then
               borders := borders + 2 * BevelWidth;
            vertical := not ((Align = alLeft) or (Align = alRight));
            dmin := 0;
            dmax := 0;
            if TitleBarVisible then
              titlewidth:=DEFAULT_TITLEBAR_WIDTH
            else
              titlewidth:=0;
            case Align of
                alLeft: begin
                    dmin := -Width+titlewidth+borders+FSplitterPanel.Width+2;
                    dmax := Form.ClientWidth - Left - Width - 50;
                    end;
                alRight: begin
                    dmin := -(Form.ClientWidth - Width - 50);
                    dmax:= Width-titlewidth-borders-2;
                    end;
                alTop: begin
                    dmin := -Height+titlewidth+borders+FSplitterPanel.Height+2; 
                    dmax := Form.ClientHeight - Top - Height - 80;
                    end;
                alBottom: begin
                    dmin := -(Form.ClientHeight - Height - 80);
                    dmax:= Form.ClientHeight - Top-titlewidth-borders-2 
                           -(Form.ClientHeight-(Top+Height)); 

                    end;
                end;
            if (dmin > 0) then
                dmin := 0;
            if (dmax < 0) then
                dmax := 0;
                
            FSplitterControl.BeginSizing( vertical, dmin, dmax, Form);
            end;
        end;
end;

procedure TEIPanel.DoSplitterMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  minsize:Integer;
  newsize:Integer;
begin
     if FSplitterControl.Sizing then begin
        if TitleBarVisible then
          minsize := DEFAULT_TITLEBAR_WIDTH
        else
          minsize := 0;
        newsize := 0;
        if (BevelInner <> bvNone) then
           minsize := minsize + 2 * BevelWidth;
        if (BevelOuter <> bvNone) then
           minsize := minsize + 2 * BevelWidth;
        if ((Align=alLeft) or (Align=alRight)) then
           minsize:=minsize+FSplitterPanel.Width
        else if ((Align=alTop) or (Align=alBottom)) then
           minsize:=minsize+FSplitterPanel.Height;
        case Align of
            alLeft:     newsize:= FSplitterControl.EndSizing          +FSplitterPanel.Width;
            alRight:    newsize:= Width - FSplitterControl.EndSizing  +FSplitterPanel.Width;
            alTop:      newsize:= FSplitterControl.EndSizing          +FSplitterPanel.Height;
            alBottom:   newsize:= Height - FSplitterControl.EndSizing +FSplitterPanel.Height;
        end;
        if (newsize<minsize) then
          newsize:=minsize;
        case Align of
            alLeft:     setbounds(Left,Top,newsize,Height);
            alRight:    setbounds(Left+Width-newsize,Top,newsize,Height);
            alTop:      setbounds(Left,Top,Width,newsize);
            alBottom:   setbounds(Left,Top+Height-newsize,Width,newsize);
        end;
        if FSplitterPanel.Visible then
          FSplitterPanel.BringToFront;
        if FTitleBar.Visible then
          FTitleBar.BringToFront;
        Refresh;
        end;
end;

procedure TEIPanel.DoSplitterMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
     if FSplitterControl.Sizing then
        FSplitterControl.ChangeSizing(X, Y);
end;

function TEIPanel.GetIDFromButton(AButton: TTitleBarButton): TTitleBarButtonID;
begin
    if (AButton = FXButton) then
        result := tbbeX
    else if (AButton = FAlignLeftButton) then
        result := tbbeLeft
    else if (AButton = FAlignRightButton) then
        result := tbbeRight
    else if (AButton = FAlignTopButton) then
        result := tbbeTop
    else if (AButton = FAlignBottomButton) then
        result := tbbeBottom
    else if (AButton = FMinMaxButton) then
        result := tbbeMinMax
    else if (AButton = FHelpButton) then
        result := tbbeHelp
    else
        result := tbbeFloat;
end;

function TEIPanel.GetTitleBarButtonFromID(ButtonID: TTitleBarButtonID): TTitleBarButton;
begin
    case ButtonID of
        tbbeX:      result := FXButton;
        tbbeLeft:   result := FAlignLeftButton;
        tbbeRight:  result := FAlignRightButton;
        tbbeTop:    result := FAlignTopButton;
        tbbeBottom: result := FAlignBottomButton;
        tbbeMinMax: result := FMinMaxButton;
        tbbeHelp:   result := FHelpButton;
        else
            result := FFloatButton;
        end;
end;

procedure TEIPanel.DoButtonClick(Sender: TObject);
var ID: TTitleBarButtonID;
    Cancel: BOOLEAN;
begin
    FLockButtonUpdate:=false;
    ID := GetIDFromButton(TTitleBarButton(Sender));
    Cancel := FALSE;
    if (Assigned(FOnButtonClick)) then begin
        FOnButtonClick(self, ID, Cancel);
        end;
    if (not Cancel) then begin
        if (ID in [tbbeLeft, tbbeRight, tbbeTop, tbbeBottom]) then begin
            case ID of
                tbbeLeft:   Align := alLeft;
                tbbeRight:  Align := alRight;
                tbbeTop:    Align := alTop;
                tbbeBottom: Align := alBottom;
                end;
            end
        else if (ID = tbbeX) then begin
            // Because we are in a complicated event here we do not free
            // directly but post a CM_EI_PanelRelease message to ourself.
            // We call the free method in the message handler of this
            // message.
            DoFree;
            end
        else if (ID = tbbeMinMax) then begin
          if ((FTitleBar.Align=alLeft) or (FTitleBar.Align=alRight)) and
             ((Align=alLeft)           or (Align=alRight)) then
            Minimized:= not Minimized
          else if ((FTitleBar.Align=alTop) or (FTitleBar.Align=alBottom)) and
                  ((Align=alTop)           or (Align=alBottom)) then
            Minimized:= not Minimized;
        end
        else if (ID = tbbeHelp) then begin
            // No default action when float button was pressed
            end
        else if (ID = tbbeFloat) then begin
            // No default action when float button was pressed
            end;
        end;
end;

function TEIPanel.GetAlign: TAlign;
begin
    result := inherited Align;
end;

procedure TEIPanel.SetAlign(Value: TAlign);
begin
  if (inherited Align <> Value) then
  begin
    if (FMinimized) and
       ((((Align=alLeft) or (Align=alRight)) and
         ((Value=alTop) or (Value=alBottom))) or
        (((Align=alTop) or (Align=alBottom)) and
         ((Value=alLeft) or (Value=alRight)))) then
    begin
      Minimized := FALSE;
      FRestoreMinimizedValue := -1;
      FRestoreMinimizedSplitter := -1;
    end;

    inherited Align := Value;
    case Value of
          alLeft:     SetSplitterAlign(talRight);
          alRight:    SetSplitterAlign(talLeft);
          alTop:      SetSplitterAlign(talBottom);
          alBottom:   SetSplitterAlign(talTop);
    end;
    UpdateMinMaxButton;
    if assigned(FOnAlign) then
      FOnAlign(Self);
  end;
end;

function TEIPanel.GetSize: Integer;
begin
    if (Align in [alLeft, alRight]) then begin
        result := Width;
        end
    else begin
        result := Height;
        end;
end;

procedure TEIPanel.SetSize(Value: Integer);
begin
    if (Align in [alLeft, alRight]) then begin
        Width := Value;
        end
    else begin
        Height := Value;
        end;
end;

function TEIPanel.GetTitleBarVisible: BOOLEAN;
begin
    result := FTitleBar.Visible;
end;

procedure TEIPanel.SetTitleBarVisible(Value: BOOLEAN);
begin
    if (FTitleBar.Visible <> Value) then begin
        FTitleBar.Visible := Value;
        SetupTitleBar;
        end;
end;

function TEIPanel.GetTitleBarColor: TColor;
begin
    result := FTitleBar.Color;
end;

procedure TEIPanel.SetTitleBarColor(Value: TColor);
begin
    if (FTitleBar.Color <> Value) then
        FTitleBar.Color := Value;
end;

function TEIPanel.GetTitleBarFont: TFont;
begin
    result := FTitleBar.Font;
end;

procedure TEIPanel.SetTitleBarFont(Value: TFont);
begin
    FTitleBar.Font.Assign(Value);
end;

procedure TEIPanel.SetTitleBarAlign(Value: TTitleBarAlign);
begin
    if (FTitleBarAlign <> Value) then begin
        if (FMinimized) then
            Minimized := FALSE;
        FRestoreMinimizedValue := -1;
        FRestoreMinimizedSplitter := -1;

        FTitleBarAlign := Value;
        SetupTitleBar;
        end;
end;

procedure TEIPanel.SetTitleBarButtonVisible(Value: TTitleBarButtonIDs);
begin
    if (FTitleBarButtonVisible <> Value) then begin
        FTitleBarButtonVisible := Value;
        SetupTitleBar;
        end;
end;

procedure TEIPanel.SetTitleBarButtonEnabled(Value: TTitleBarButtonIDs);
begin
    if (FTitleBarButtonEnabled <> Value) then begin
        FTitleBarButtonEnabled := Value;
        SetupTitleBar;
        end;
end;

procedure TEIPanel.Resize;
begin
    inherited Resize;
    SetupTitlebar;
end;

function TEIPanel.GetTitleBarCaption: TCaption;
begin
    result := FTitleBar.Caption;
end;

procedure TEIPanel.SetTitleBarCaption(Value: TCaption);
begin
    if (FTitleBar.Caption <> Value) then begin
        FTitleBar.Caption := Value;
        end;
end;

function TEIPanel.GetTitleBarHint:String;
begin
  result:=FTitleBar.Hint;
end;

procedure TEIPanel.SetTitleBarHint(Value:String);
begin
  if (FTitleBar.Hint<>Value) then
    FTitleBar.Hint:=Value;
end;

function TEIPanel.GetTitleBarPopupMenu:TPopupMenu;
begin
  result:=FTitleBar.PopupMenu;
end;

procedure TEIPanel.SetTitleBarPopupMenu(Value:TPopupMenu);
begin
  if (FTitleBar.PopupMenu<>Value) then
    FTitleBar.PopupMenu:=Value;
end;

{Method_Marker(TEIPanel.GetDragable)}

function TEIPanel.GetDragable: BOOLEAN;
begin
    result := FTitleBar.DragAllowed;
end;


{Method_Marker(TEIPanel.SetDragable)}

procedure TEIPanel.SetDragable(Value: BOOLEAN);
begin
    FTitleBar.DragAllowed := Value;
end;

function TEIPanel.GetSplitterAlign: TTitleBarAlign;
begin
    case FSplitterPanel.Align of
        alLeft: result := talLeft;
        alRight: result := talRight;
        alTop: result := talTop;
        alBottom: result := talBottom
        else result := talLeft;
        end;
end;

const
    SPLITTER_WIDTH = 4;

procedure TEIPanel.SetSplitterAlign(Value: TTitleBarAlign);
begin
    case Value of
        talLeft: begin FSplitterPanel.Align := alLeft;
                       FSplitterPanel.Left:=0; end;
        talRight: begin FSplitterPanel.Align := alRight;
                       FSplitterPanel.Left:=Width; end;
        talTop: begin FSplitterPanel.Align := alTop;
                       FSplitterPanel.Top:=0; end;
        talBottom: begin FSplitterPanel.Align := alBottom;
                       FSplitterPanel.Top:=Height; end;
        end;
    if ((Value = talLeft) or (Value = talRight)) then begin
        FSplitterPanel.Cursor := crHSplit;
        FSplitterPanel.Width := SPLITTER_WIDTH;
        end
    else begin
        FSplitterPanel.Cursor := crVSplit;
        FSplitterPanel.Height := SPLITTER_WIDTH;
        end;
end;

function TEIPanel.GetSplitterVisible: BOOLEAN;
begin
    result := FSplitterPanel.Visible;
end;

procedure TEIPanel.SetSplitterVisible(Value: BOOLEAN);
begin
    FSplitterPanel.Visible := Value;
    if FSplitterPanel.Visible then
    case FSplitterPanel.Align of
      alLeft:   FSplitterPanel.Left:=0;
      alRight:  FSplitterPanel.Left:=Width;
      alTop:    FSplitterPanel.Top:=0;
      alBottom: FSplitterPanel.Top:=Height;
    end;
end;

function TEIPanel.GetSplitterColor:TColor;
begin
  result:=FSplitterPanel.Color;
end;

procedure TEIPanel.SetSplitterColor(Value:TColor);
begin
  FSplitterPanel.Color:=Value;
end;

function TEIPanel.GetSplitterBevel:TPanelBevel;
begin
  result:=FSplitterPanel.BevelOuter;
end;

procedure TEIPanel.SetSplitterBevel(Value:TPanelBevel);
begin
  FSplitterPanel.BevelOuter:=Value;
end;

{Method_Marker(TEIPanel.SetMinimized)}

procedure TEIPanel.SetMinimized(Value: BOOLEAN);
var borders: Integer;
begin
 if (FMinimized<>Value) and
    (IsTitleBarVertical=IsVertical) then
 begin
   FMinimized := Value;
   if (FMinMaxButton<>nil) then
   begin
     if FMinimized then
       FMinMaxButton.Glyph.LoadFromResourceName(HInstance, 'EDITOR_INTEGRATION_PANEL_BTN_MAXIMIZE')
     else
       FMinMaxButton.Glyph.LoadFromResourceName(HInstance, 'EDITOR_INTEGRATION_PANEL_BTN_MINIMIZE');
   end;
   if ((not FMinimized) and (FRestoreMinimizedValue <> -1)) then
   begin
     if ((Align = alTop) or (Align = alBottom)) then
     begin
       if Align=alBottom then
         setbounds(Left,Top+Height-FRestoreMinimizedValue,Width,FRestoreMinimizedValue)
       else
         setbounds(Left,Top,Width,FRestoreMinimizedValue)
     end
     else if ((Align = alLeft) or (Align = alRight)) then
     begin
       if Align=alRight then
         setbounds(Left+Width-FRestoreMinimizedValue,Top,FRestoreMinimizedValue,Height)
       else
         setbounds(Left,Top,FRestoreMinimizedValue,Height)
     end;
     if (FRestoreMinimizedSplitter = 1) then
     begin
       SplitterVisible := TRUE;
     end;
   end
   else
   begin
     if (SplitterVisible) then
     begin
       FRestoreMinimizedSplitter := 1;
     end
     else
     begin
       FRestoreMinimizedSplitter := 0;
     end;
     borders := 0;
     if (BevelInner <> bvNone) then
       borders := 2 * BevelWidth;
     if (BevelOuter <> bvNone) then
       borders := borders + 2 * BevelWidth;
     if ((Align = alTop) or (Align = alBottom)) then
     begin
       FRestoreMinimizedValue := Height;
       SplitterVisible := FALSE;
       Height := FTitleBar.Height + borders;
       FTitleBar.BringToFront;
       if FTitleBar.Visible then
       begin
         FTitleBar.Visible:=false;
         FTitleBar.Visible:=true;
       end;
     end
     else if ((Align = alLeft) or (Align = alRight)) then
     begin
       FRestoreMinimizedValue := Width;
       SplitterVisible := FALSE;
       Width := FTitleBar.Width + borders;
       FTitleBar.BringToFront;
       if FTitleBar.Visible then
       begin
         FTitleBar.Visible:=false;
         FTitleBar.Visible:=true;
       end;
     end;
   end;
   refresh;
 end;
end;

function TEIPanel.GetButtonHint(ButtonID: TTitleBarButtonID): String;
var button: TTitleBarButton;
begin
    button := GetTitleBarButtonFromID(ButtonID);
    if (button <> nil) then begin
        result := button.Hint;
        end
    else begin
        result := '';
        end;
end;


procedure TEIPanel.SetButtonHint(ButtonID: TTitleBarButtonID; Value: String);
var button: TTitleBarButton;
begin
    button := GetTitleBarButtonFromID(ButtonID);
    if (button <> nil) then begin
        button.Hint := Value;
        end;
end;

function TEIPanel.GetTitleBarDrawEvent:TTitleBarDrawEvent;
begin
  result:=FTitleBar.OnPaint;
end;

procedure TEIPanel.SetTitleBarDrawEvent(Value:TTitleBarDrawEvent);
begin
  FTitleBar.OnPaint:=Value;
end;

function TEIPanel.GetTitleBarIcon:TIcon;
begin
  result:=FTitleBar.Icon;
end;

procedure TEIPanel.SetTitleBarIcon(Value:TIcon);
begin
  FTitleBar.Icon:=Value;
end;

procedure TEIPanel.UpdateMinMaxButton;
begin
  if FMinMaxButton<>nil then
  begin
    if ((FTitleBar.Align=alLeft) or (FTitleBar.Align=alRight)) and
       ((Align=alLeft)           or (Align=alRight)) then
      FMinMaxButton.Enabled:=true
    else if ((FTitleBar.Align=alTop) or (FTitleBar.Align=alBottom)) and
            ((Align=alTop)           or (Align=alBottom)) then
      FMinMaxButton.Enabled:=true
    else
      FMinMaxButton.Enabled:=false;
  end;
  if FMinMaxButton.Enabled then
    FTitleBarButtonEnabled:=FTitleBarButtonEnabled+[tbbeMinMax]
  else
    FTitleBarButtonEnabled:=FTitleBarButtonEnabled-[tbbeMinMax];
end;

//Method_Marker(TEIPanel.SetTitleBarButtonFlat)

procedure TEIPanel.SetTitleBarButtonFlat(Value: BOOLEAN);
begin
    if (FTitleBarButtonFlat <> Value) then begin
        FTitleBarButtonFlat := Value;
        SetupTitleBar;
    end;
end;

//Method_Marker(TEIPanel.IsTitleBarVertical)

function TEIPanel.IsTitleBarVertical: BOOLEAN;
begin
  result:=(FTitleBar.Align=alLeft) or (FTitleBar.Align=alRight);
end;

function TEIPanel.AddButton:TTitleBarBtn;
var
  Button:TTitleBarButton;
begin
  result:=TTitleBarBtn.Create(Self);
  FTitleBarBtnList.Add(result);
  Button := TTitleBarButton.Create(FTitleBar);
  Button.Parent := FTitleBar;
  Button.Visible := false;
  Button.Width := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
  Button.Height := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
  Button.OnClick:=DoTitleBarButtonClick;
  FTitleBarButtonList.Add(Button);
end;

function TEIPanel.InsertButton(Index:Integer):TTitleBarBtn;
var
  Button:TTitleBarButton;
begin
  result:=TTitleBarBtn.Create(Self);
  FTitleBarBtnList.Insert(Index,result);
  Button := TTitleBarButton.Create(FTitleBar);
  Button.Parent := FTitleBar;
  Button.Visible := false;
  Button.Width := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
  Button.Height := DEFAULT_TITLEBAR_BUTTONS_WIDTH;
  FTitleBarButtonList.Insert(Index,Button);
end;

function TEIPanel.GetIndexFromID(ButtonID:Integer):Integer;
var
  i:Integer;
begin
  result:=-1;
  for i:=0 to ButtonCount-1 do
    if TTitleBarBtn(FTitleBarBtnList[i]).ButtonId=ButtonID then
    begin
      result:=i;
      break;
    end;
end;

function TEIPanel.GetTitleBarBtn(Index:Integer):TTitleBarBtn;
begin
  result:=nil;
  if Index<ButtonCount then
    result:=TTitleBarBtn(FTitleBarBtnList[Index]);
end;

function TEIPanel.GetTitleBarBtnFromID(ButtonId:Integer):TTitleBarBtn;
var
  i:Integer;
begin
  result:=nil;
  for i:=0 to ButtonCount-1 do
    if TTitleBarBtn(FTitleBarBtnList[i]).ButtonId=ButtonID then
    begin
      result:=TTitleBarBtn(FTitleBarBtnList[i]);
      break;
    end;
end;

function TEIPanel.ButtonCount:Integer;
begin
  result:=FTitleBarButtonList.Count;
end;

procedure TEIPanel.DeleteButton(Index:Integer);
begin
  if Index<ButtonCount then
  begin
    TTitleBarButton(FTitleBarButtonList[Index]).Free;
    TTitleBarBtn(FTitleBarBtnList[Index]).Free;
    FTitleBarButtonList.Delete(Index);
    FTitleBarBtnList.Delete(Index);
    SetupTitleBar;
  end;
end;

procedure TEIPanel.ClearButtons;
begin
  while ButtonCount>0 do
    DeleteButton(ButtonCount-1);
  SetupTitleBar;
end;

procedure TEIPanel.ExchangeButtons(Index1,Index2:Integer);
begin
  if (Index1<ButtonCount) and (Index2<ButtonCount) then
  begin
    FTitleBarButtonList.Exchange(Index1,Index2);
    FTitleBarBtnList.Exchange(Index1,Index2);
    SetupTitleBar;
  end;
end;

procedure TEIPanel.LockButtonUpdate;
begin
  FLockButtonUpdate:=true;
end;

procedure TEIPanel.UnlockButtonUpdate;
begin
  FLockButtonUpdate:=false;
  SetupTitleBar;
end;

//Method_Marker(TEIPanel.GetTitleBarButton)

function TEIPanel.GetTitleBarButton(index: integer): TTitleBarButton;
begin
  result:=nil;
  if Index<ButtonCount then
    result:=TTitleBarButton(FTitleBarButtonList[Index]);
end;

//Method_Marker(TEIPanel.TitleBarHeight)

function TEIPanel.TitleBarHeight: Integer;
begin
  if IsTitleBarVertical then
    result:=FTitleBar.Width
  else
    result:=FTitleBar.Height;
end;

//Method_Marker(TEIPanel.GetTitleBarCanvas)

function TEIPanel.GetTitleBarCanvas: TCanvas;
begin
  result:=FTitleBar.Canvas;
end;

//Method_Marker(TEIPanel.TitleBarClientRect)

function TEIPanel.TitleBarClientRect: TRect;
begin
  result:=FTitleBar.ClientRect;
end;

//Method_Marker(TEIPanel.GetTitleBarFilling)

function TEIPanel.GetTitleBarFilling: Boolean;
begin
    result := FTitleBar.Filling;
end;

//Method_Marker(TEIPanel.SetTitleBarFilling)

procedure TEIPanel.SetTitleBarFilling(Value: Boolean);
begin
    if (FTitleBar.Filling <> Value) then begin
        FTitleBar.Filling := Value;
    end;
end;

//Method_Marker(TEIPanel.IsVertical)

function TEIPanel.IsVertical: Boolean;
begin
  result:=(Align=alLeft) or (Align=alRight);
end;

//Method_Marker(TEIPanel.TitleBarVisibleWidth)

function TEIPanel.TitleBarVisibleWidth: Integer;
begin
  result:=FNextButtonPos;
  if (not IsTitleBarVertical) then
    result:=result+DEFAULT_TITLEBAR_BUTTONS_WIDTH;
  if ((result<0) and (not IsTitleBarVertical)) or
     ((result>FTitleBar.Height) and (IsTitleBarVertical)) then
    result:=0;
end;

{ClassMarker_Method(TEIPanel)}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Service Routines
////////////////////////////////////////////////////////////////////////////////////////////////////

function CToC(C1, C2: TControl; Point: TPoint): TPoint;
begin
    Result := C1.ScreenToClient(C2.ClientToScreen(Point));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TDragControl
////////////////////////////////////////////////////////////////////////////////////////////////////

procedure TDragControl.DrawDragRect;
begin
  FControlCanvas.DrawFocusRect(FDrawDragRect);
end;

procedure TDragControl.BeginDragging(ADragControl: TControl; CaptureControl: TControl; StartPoint: TPoint{; Form: TCustomForm});
begin
    FDragTarget := ADragControl;

    FDragRect.TopLeft := FDragTarget.ClientToScreen(Point(0, 0));
    FDragRect.BottomRight := FDragTarget.ClientToScreen(Point(FDragTarget.Width, FDragTarget.Height));
    FDrawDragRect := FDragRect;

    FStartPoint := FDragTarget.ClientToScreen(StartPoint);
    FDragPoint := FStartPoint;

    SetCaptureControl(CaptureControl);

    FControlCanvas := TControlCanvas.Create;
    FControlCanvas.Handle := GetDCEx(GetDesktopWindow, 0, DCX_CACHE or DCX_CLIPSIBLINGS or DCX_LOCKWINDOWUPDATE);
    FControlCanvas.Pen.Mode := pmXOR;
    FControlCanvas.Pen.Width := 4;
    DrawDragRect;
    FDragging:=true;
end;

procedure TDragControl.ChangeDragging(DragPoint: TPoint);
var dX,dY: Integer;
begin
    DrawDragRect;

    FDragPoint := FDragTarget.ClientToScreen(DragPoint);

    dX := FDragPoint.X - FStartPoint.X;
    dY := FDragPoint.Y - FStartPoint.Y;

    FDrawDragRect.Left := FDragRect.Left + dX;
    FDrawDragRect.Right := FDragRect.Right + dX;
    FDrawDragRect.Top := FDragRect.Top + dY;
    FDrawDragRect.Bottom := FDragRect.Bottom + dY;

    DrawDragRect;
end;

function TDragControl.EndDragging: TPoint;
begin
  if FDragging then
  begin
    FDragging:=false;
    DrawDragRect;

    SetCaptureControl(nil);
    FDragTarget := nil;
    FControlCanvas.FreeHandle;
    FControlCanvas.Free;
  end;

    result.X := FDragPoint.X - FStartPoint.X;
    result.Y := FDragPoint.Y - FStartPoint.Y;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// TSplitterControl
////////////////////////////////////////////////////////////////////////////////////////////////////

constructor TSplitterControl.Create(ASplitControl, ATargetControl: TControl);
begin
    inherited Create;
    FSplitControl := ASplitControl;
    FSizeTarget := ATargetControl;
    FForm := nil;
end;

function TSplitterControl.GetSizing: Boolean;
begin
  Result := FForm <> nil;
end;

procedure TSplitterControl.DrawSizingLine;
var P: TPoint;
begin
    P := CToC(FForm, FSplitControl, FSplit);
    with FForm.Canvas do begin
        MoveTo(P.X, P.Y);
        if FVertical then
            LineTo(CToC(FForm, FSplitControl, Point(FSplitControl.Width, 0)).X, P.Y)
        else
            LineTo(P.X, CToC(FForm, FSplitControl, Point(0, FSplitControl.Height)).Y)
        end;
end;

procedure TSplitterControl.BeginSizing(Vertical: BOOLEAN; DMIN: Integer; DMax: Integer; Form: TCustomForm);
begin
    FForm := Form;
    FMin := Dmin;
    FMax := DMax;

    SetCaptureControl(FSplitControl);
    FVertical := Vertical; 
    if FVertical then
     FSplit := Point(0, FSplitControl.Top)
    else
     FSplit := Point(FSplitControl.Left, 0);

    FForm.Canvas.Handle := GetDCEx(FForm.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS or DCX_LOCKWINDOWUPDATE);
    with FForm.Canvas do begin
        Pen.Color := clWhite;
        if FVertical then
         Pen.Width := FSplitControl.Height
        else
         Pen.Width := FSplitControl.Width;

        Pen.Mode := pmXOR;
        end;
    DrawSizingLine;
end;

procedure TSplitterControl.ChangeSizing(X, Y: Integer);
begin
    DrawSizingLine;
    if FVertical then begin
        if (Y < FMin) then
            Y := FMin
        else if (Y > FMax) then
            Y := FMax;
        end
    else begin
        if (X < FMin) then
            X := FMin
        else if (X > FMax) then
            X := FMax;
        end;

    if FVertical then FSplit.Y := Y else FSplit.X := X;
    DrawSizingLine;
end;

function TSplitterControl.EndSizing: Integer;
var
  DC: HDC;
  P: TPoint;
begin
    DrawSizingLine;
    P := CToC(FSizeTarget, FSplitControl, FSplit);
    SetCaptureControl(nil);
    with FForm do begin
        DC := Canvas.Handle;
        Canvas.Handle := 0;
        ReleaseDC(Handle, DC);
        end;
    if FVertical then
        result := P.Y
    else
        result := P.X;

    FForm := nil;
end;

end.

