{ *******************************************************************************
*                                                                              *
*  ksStripe - Stripe Interface for Delphi                                      *
*                                                                              *
*  https://github.com/gmurt/ksStripe                                           *
*                                                                              *
*  Copyright 2015 Graham Murt                                                  *
*                                                                              *
*  email: graham@kernow-software.co.uk                                         *
*                                                                              *
*  Licensed under the Apache License, Version 2.0 (the "License");             *
*  you may not use this file except in compliance with the License.            *
*  You may obtain a copy of the License at                                     *
*                                                                              *
*    http://www.apache.org/licenses/LICENSE-2.0                                *
*                                                                              *
*  Unless required by applicable law or agreed to in writing, software         *
*  distributed under the License is distributed on an "AS IS" BASIS,           *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    *
*  See the License for the specific language governing permissions and         *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}

unit ksStripe;

interface

uses Classes, Json, Generics.Collections;

type
  TStripeCurrency = (scUnknown, scGbp, scUsd);

//------------------------------------------------------------------------------

  IStripeBaseObject = interface
  ['{AC396FFE-A89C-4811-8DDD-5A3A69546155}']
    function GetID: string;
    function GetObject: string;
    procedure LoadFromJson(AJson: TJsonObject);
    property ID: string read GetID;
    property Obj: string read GetObject;
  end;

//------------------------------------------------------------------------------

  IStripeBaseObjectList = interface
  ['{3FD36F72-3FF3-4377-AE0E-120A19C63354}']
    function GetCount: integer;
    function GetItem(index: integer): IStripeBaseObject;
    function GetListID: string;
    procedure Clear;
    procedure LoadFromJson(AJson: TJSONObject);
    property Count: integer read GetCount;
    property Item[index: integer]: IStripeBaseObject read GetItem;
  end;

//------------------------------------------------------------------------------

  IStripeCharge = interface(IStripeBaseObject)
  ['{197B9D1A-B4F1-4220-AFDC-22DE5031F1B4}']
    function GetCreated: TDatetime;
    function GetLiveMode: Boolean;
    function GetPaid: Boolean;
    function GetStatus: string;
    function GetAmountPence: integer;
    function GetCurrency: TStripeCurrency;
    function GetRefunded: Boolean;
    property Created: TDateTime read GetCreated;
    property LiveMode: Boolean read GetLiveMode;
    property Paid: Boolean read GetPaid;
    property Status: string read GetStatus;
    property AmountPence: integer read GetAmountPence;
    property Currency: TStripeCurrency read GetCurrency;
    property Refunded: Boolean read GetRefunded;
  end;

//------------------------------------------------------------------------------

  IStripePlan = interface(IStripeBaseObject)
  ['{E37D8D42-0FDE-4108-BD58-56603955FDCC}']
    function GetAmountPence: integer;
    function GetCreated: TDateTime;
    function GetCurrency: TStripeCurrency;
    function GetInterval: string;
    function GetIntervalCount: integer;
    function GetName: string;
    function GetStatementDescriptor: string;
    function GetTrialPeriodDays: integer;
    property Interval: string read GetInterval;
    property Name: string read GetName;
    property Created: TDateTime read GetCreated;
    property AmountPence: integer read GetAmountPence;
    property Currency: TStripeCurrency read GetCurrency;
    property IntervalCount: integer read GetIntervalCount;
    property TrialPeriodDays: integer read GetTrialPeriodDays;
    property StatementDescriptor: string read GetStatementDescriptor;
  end;

//------------------------------------------------------------------------------

  IStripeSubscription = interface(IStripeBaseObject)
  ['{3F2BE016-7483-4020-BEB6-F0A3B55E9753}']
    function GetCancelledAt: TDateTime;
    function GetCurrentPeriodEnd: TDateTime;
    function GetCurrentPeriodStart: TDateTime;
    function GetCustomer: string;
    function GetEndedAt: TDateTime;
    function GetPlan: IStripePlan;
    function GetQuantity: integer;
    function GetStart: TDateTime;
    function GetStatus: string;
    function GetTaxPercent: single;
    function GetTrialEnd: TDateTime;
    function GetTrialStart: TDateTime;
    property Plan: IStripePlan read GetPlan;
    property Start: TDateTime read GetStart;
    property Status: string read GetStatus;
    property Customer: string read GetCustomer;
    property CurrentPeriodStart: TDateTime read GetCurrentPeriodStart;
    property CurrentPeriodEnd: TDateTime read GetCurrentPeriodEnd;
    property EndedAt: TDateTime read GetEndedAt;
    property TrialStart: TDateTime read GetTrialStart;
    property TrialEnd: TDateTime read GetTrialEnd;
    property CancelledAt: TDateTime read GetCancelledAt;
    property Quantity: integer read GetQuantity;
    property TaxPercent: single read GetTaxPercent;
  end;

//------------------------------------------------------------------------------

  IStripeSubscriptionList = interface(IStripeBaseObjectList)
  ['{27861C97-3F5F-4546-9CAE-1248040E5159}']
  end;

//------------------------------------------------------------------------------

  IStripeCustomer = interface(IStripeBaseObject)
  ['{CFA07B51-F63C-4972-ACAB-FA51D6DF5779}']
    function GetAccountBalance: integer;
    function GetCurrency: TStripeCurrency;
    function GetEmail: string;
    function GetDescription: string;
    procedure SetEmail(const Value: string);
    procedure SetCurrency(const Value: TStripeCurrency);
    procedure SetAccountBalance(const Value: integer);
    procedure SetDescription(const Value: string);
    property Email: string read GetEmail write SetEmail;
    property Description: string read GetDescription write SetDescription;
    property Currency: TStripeCurrency read GetCurrency write SetCurrency;
    property AccountBalance: integer read GetAccountBalance write SetAccountBalance;
  end;

//------------------------------------------------------------------------------

  IStripeCusotomerList = interface(IStripeBaseObjectList)
  ['{A84D8E11-C142-4E4C-9698-A6DFBCE14742}']
  end;

//------------------------------------------------------------------------------

  IStripeCard = interface(IStripeBaseObject)
  ['{76652D56-42CE-4C2F-B0B2-1E6485D501AD}']
    function GetBrand: string;
    function GetFunding: string;
    function GetLast4: string;
    function GetAddress1: string;
    function GetCountry: string;
    function GetExpMonth: integer;
    function GetExpYear: integer;
    function GetName: string;
    function GetAddress2: string;
    function GetCity: string;
    function GetState: string;
    function GetZip: string;
    function GetAddressCountry: string;
    function GetCvcCheck: string;
    property Last4: string read GetLast4;
    property Brand: string read GetBrand;
    property Funding: string read GetFunding;
    property ExpMonth: integer read GetExpMonth;
    property ExpYear: integer read GetExpYear;
    property Country: string read GetCountry;
    property Name: string read GetName;
    property Address1: string read GetAddress1;
    property Address2: string read GetAddress2;
    property City: string read GetCity;
    property State: string read GetState;
    property Zip: string read GetZip;
    property AddressCountry: string read GetAddressCountry;
    property CvcCheck: string read GetCvcCheck;
  end;

//------------------------------------------------------------------------------

  IStripe = interface
  ['{A00E2188-0DDB-469F-9C4A-0900DEEFD27B}']
    function GetLastError: string;
    function CreateToken(ACardNum: string; AExpMonth, AExpYear: integer; ACvc: string): string;
    function CreateCharge(AToken, ADescription: string; AAmountPence: integer; const ACurrency: TStripeCurrency = scGbp): IStripeCharge;
    function CreateChargeForCustomer(ACustID, ADescription: string; AAmountPence: integer; const ACurrency: TStripeCurrency = scGbp): IStripeCharge;
    function GetCustomer(ACustID: string): IStripeCustomer;
    function GetCustomers: IStripeCusotomerList;
    function CreateCustomer(AEmail, ADescription: string; ABalancePence: integer): IStripeCustomer;
    property LastError: string read GetLastError;
  end;

//------------------------------------------------------------------------------



  function CreateStripe(ASecretKey: string): IStripe;
  function CreateStripeCustomer: IStripeCustomer;

implementation

uses System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  SysUtils, DateUtils;

const
  C_CARD = 'card';
  C_CARDS = 'cards';
  C_CHARGE = 'charge';
  C_CHARGES = 'charges';
  C_CUSTOMER = 'customer';
  C_CUSTOMERS = 'customers';
  C_TOKEN  = 'token';
  C_TOKENS = 'tokens';
  C_PLAN = 'plan';
  C_PLANS = 'plans';
  C_SUBSCRIPTION = 'subscription';
  C_SUBSCRIPTIONS = 'subscriptions';

type
//------------------------------------------------------------------------------

  TStripeBaseObject = class(TInterfacedObject, IStripeBaseObject)
  strict private
    FJson: TJSONObject;
    FId: string;
    FObj: string;
  protected
    function StrFromJson(AName: string): string;
    function DateFromJson(AName: string): TDateTime;
    function BoolFromJson(AName: string): Boolean;
    function IntFromJson(AName: string): integer;
    function GetID: string;
    function GetObj: string;
    function GetObject: string; virtual; abstract;
    procedure LoadFromJson(AJson: TJsonObject); virtual;
    property ID: string read GetID;
    property Obj: string read GetObj;
  public
    constructor Create; virtual;
  end;

//------------------------------------------------------------------------------

  TStripeBaseObjectList = class(TInterfacedObject, IStripeBaseObjectList)
  strict private
    FItems: TList<TStripeBaseObject>;
  private
    function GetCount: integer;
  protected
    constructor Create; virtual;
    function CreateObject: IStripeBaseObject; virtual; abstract;
    function AddObject: IStripeBaseObject; virtual;
    function GetListID: string; virtual; abstract;
    procedure Clear;
    procedure LoadFromJson(AJson: TJSONObject);
    function GetItem(index: integer): IStripeBaseObject;
    property Count: integer read GetCount;
    property Item[index: integer]: IStripeBaseObject read GetItem;
  end;

//------------------------------------------------------------------------------

  TStripeCharge = class(TStripeBaseObject, IStripeCharge)
  strict private
    FCreated: TDateTime;
    FLiveMode: Boolean;
    FPaid: Boolean;
    FStatus: string;
    FAmountPence: integer;
    FCurrency: TStripeCurrency;
    FRefunded: Boolean;
  private
    function GetCreated: TDatetime;
    function GetLiveMode: Boolean;
    function GetPaid: Boolean;
    function GetStatus: string;
    function GetAmountPence: integer;
    function GetCurrency: TStripeCurrency;
    function GetRefunded: Boolean;
  protected
    function GetObject: string; override;
    procedure LoadFromJson(AJson: TJsonObject); override;
    property Created: TDateTime read GetCreated;
    property LiveMode: Boolean read GetLiveMode;
    property Paid: Boolean read GetPaid;
    property Status: string read GetStatus;
    property AmountPence: integer read GetAmountPence;
    property Currency: TStripeCurrency read GetCurrency;
    property Refunded: Boolean read GetRefunded;
  end;

//------------------------------------------------------------------------------

  TStripeCard = class(TStripeBaseObject, IStripeCard)
  strict private
    FBrand: string;
    FFunding: string;
    FLast4: string;
    FExpMonth: integer;
    FExpYear: integer;
    FCountry: string;
    FName: string;
    FAddress1: string;
    FAddress2: string;
    FCity: string;
    FState: string;
    FZip: string;
    FAddressCountry: string;
    FCvcCheck: string;
  private
    function GetBrand: string;
    function GetFunding: string;
    function GetLast4: string;
    function GetAddress1: string;
    function GetCountry: string;
    function GetExpMonth: integer;
    function GetExpYear: integer;
    function GetName: string;
    function GetAddress2: string;
    function GetCity: string;
    function GetState: string;
    function GetZip: string;
    function GetAddressCountry: string;
    function GetCvcCheck: string;
  protected
    function GetObject: string; override;
  public
    property Last4: string read GetLast4;
    property Brand: string read GetBrand;
    property Funding: string read GetFunding;
    property ExpMonth: integer read GetExpMonth;
    property ExpYear: integer read GetExpYear;
    property Country: string read GetCountry;
    property Name: string read GetName;
    property Address1: string read GetAddress1;
    property Address2: string read GetAddress2;
    property City: string read GetCity;
    property State: string read GetState;
    property Zip: string read GetZip;
    property AddressCountry: string read GetAddressCountry;
    property CvcCheck: string read GetCvcCheck;
  end;

//------------------------------------------------------------------------------

  TStripePlan = class(TStripeBaseObject, IStripePlan)
  strict private
    FInterval: string;
    FName: string;
    FCreated: TDateTime;
    FAmountPence: integer;
    FCurrency: TStripeCurrency;
    FIntervalCount: integer;
    FTrialPeriodDays: integer;
    FStatementDescriptor: string;
  private
    function GetAmountPence: integer;
    function GetCreated: TDateTime;
    function GetCurrency: TStripeCurrency;
    function GetInterval: string;
    function GetIntervalCount: integer;
    function GetName: string;
    function GetStatementDescriptor: string;
    function GetTrialPeriodDays: integer;
  protected
    function GetObject: string; override;
    procedure LoadFromJson(AJson: TJsonObject); override;
    property Interval: string read GetInterval;
    property Name: string read GetName;
    property Created: TDateTime read GetCreated;
    property AmountPence: integer read GetAmountPence;
    property Currency: TStripeCurrency read GetCurrency;
    property IntervalCount: integer read GetIntervalCount;
    property TrialPeriodDays: integer read GetTrialPeriodDays;
    property StatementDescriptor: string read GetStatementDescriptor;
  end;

//------------------------------------------------------------------------------

  TStripeSubscription = class(TStripeBaseObject, IStripeSubscription)
  strict private
    FPlan: IStripePlan;
    FStart: TDateTime;
    FStatus: string;
    FCustomer: string;
    FCurrentPeriodStart: TDateTime;
    FCurrentPeriodEnd: TDateTime;
    FEndedAt: TDateTime;
    FTrialStart: TDateTime;
    FTrialEnd: TDateTime;
    FCancelledAt: TDateTime;
    FQuantity: integer;
    FTaxPercent: Single;
  private
    function GetCancelledAt: TDateTime;
    function GetCurrentPeriodEnd: TDateTime;
    function GetCurrentPeriodStart: TDateTime;
    function GetCustomer: string;
    function GetEndedAt: TDateTime;
    function GetPlan: IStripePlan;
    function GetQuantity: integer;
    function GetStart: TDateTime;
    function GetStatus: string;
    function GetTaxPercent: single;
    function GetTrialEnd: TDateTime;
    function GetTrialStart: TDateTime;
  protected
    function GetObject: string; override;
    procedure LoadFromJson(AJson: TJsonObject); override;
  public
    constructor Create; override;
    property Plan: IStripePlan read GetPlan;
    property Start: TDateTime read GetStart;
    property Status: string read GetStatus;
    property Customer: string read GetCustomer;
    property CurrentPeriodStart: TDateTime read GetCurrentPeriodStart;
    property CurrentPeriodEnd: TDateTime read GetCurrentPeriodEnd;
    property EndedAt: TDateTime read GetEndedAt;
    property TrialStart: TDateTime read GetTrialStart;
    property TrialEnd: TDateTime read GetTrialEnd;
    property CancelledAt: TDateTime read GetCancelledAt;
    property Quantity: integer read GetQuantity;
    property TaxPercent: single read GetTaxPercent;
  end;

//------------------------------------------------------------------------------

  TStripeSubscriptionList = class(TStripeBaseObjectList, IStripeSubscriptionList)
  protected
    function CreateObject: IStripeBaseObject; override;
    function GetListID: string; override;
  end;

//------------------------------------------------------------------------------

  TStripeCustomer = class(TStripeBaseObject, IStripeCustomer)
  strict private
    FEmail: string;
    FDescription: string;
    FCurrency: TStripeCurrency;
    FAccountBalance: integer;
    FSubscriptions: IStripeSubscriptionList;
  private
    function GetAccountBalance: integer;
    function GetCurrency: TStripeCurrency;
    function GetEmail: string;
    procedure SetAccountBalance(const Value: integer);
    procedure SetCurrency(const Value: TStripeCurrency);
    procedure SetEmail(const Value: string);
    function GetDescription: string;
    procedure SetDescription(const Value: string);
  protected
    function GetObject: string; override;
    procedure LoadFromJson(AJson: TJsonObject); override;
    property Email: string read GetEmail write SetEmail;
    property Currency: TStripeCurrency read GetCurrency write SetCurrency;
    property AccountBalance: integer read GetAccountBalance write SetAccountBalance;
    property Description: string read GetDescription write SetDescription;
  public
    constructor Create; override;
  end;

//------------------------------------------------------------------------------

  TStripeCustomerList = class(TStripeBaseObjectList, IStripeCusotomerList)
  protected
    function CreateObject: IStripeBaseObject; override;
    function GetListID: string; override;
  end;


//------------------------------------------------------------------------------

  TStripe = class(TInterfacedObject, IStripe)
  strict private
    FSecretKey: string;
    FLastError: string;
  private
    procedure CheckForError(AJson: TJsonObject);
    procedure NetHTTPClient1AuthEvent(const Sender: TObject;
                                      AnAuthTarget: TAuthTargetType;
                                      const ARealm, AURL: string; var AUserName,
                                      APassword: string; var AbortAuth: Boolean;
                                      var Persistence: TAuthPersistenceType);
    function CreateHttp: TNetHTTPClient;
    function GetHttp(AMethod: string): string;
    function PostHttp(AToken, AMethod: string; AParams: TStrings): string;
    function GetLastError: string;
  protected
    function CreateToken(ACardNum: string; AExpMonth, AExpYear: integer; ACvc: string): string;
    function CreateCharge(AToken, ADescription: string; AAmountPence: integer; const ACurrency: TStripeCurrency = scGbp): IStripeCharge;
    function CreateChargeForCustomer(ACustID, ADescription: string; AAmountPence: integer; const ACurrency: TStripeCurrency = scGbp): IStripeCharge;
    function GetCustomer(ACustID: string): IStripeCustomer;
    function GetCustomers: IStripeCusotomerList;
    function CreateCustomer(AEmail, ADescription: string; ABalancePence: integer): IStripeCustomer;
    property LastError: string read GetLastError;
  public
    constructor Create(ASecretKey: string);
  end;

//------------------------------------------------------------------------------


function  CreateStripe(ASecretKey: string): IStripe;
begin
  Result := TStripe.Create(ASecretKey);
end;

function CreateStripeCustomer: IStripeCustomer;
begin
  Result := TStripeCustomer.Create;
end;

function CurrencyToString(ACurrency: TStripeCurrency): string;
begin
  Result := '';
  case ACurrency of
    scGbp: Result := 'gbp';
    scUsd: Result := 'usd';
  end;
end;

function StringToCurrency(AValue: string): TStripeCurrency;
begin
  AValue := LowerCase(AValue);
  Result := scUnknown;
  if AValue = 'gbp' then Result := scGbp;
  if AValue = 'usd' then Result := scUsd;
end;

//------------------------------------------------------------------------------

{ TStripe }

constructor TStripe.Create(ASecretKey: string);
begin
  inherited Create;
  FSecretKey := ASecretKey;
end;

procedure TStripe.CheckForError(AJson: TJsonObject);
var
  AError: TJSONObject;
begin
  if AJson.Values['error'] <> nil then
  begin
    AError := AJson.Values['error'] as TJSONObject;
    FLastError := AError.Values['message'].Value;
    raise Exception.Create(FLastError);
  end;
end;

function TStripe.CreateCharge(AToken, ADescription: string; AAmountPence: integer; const ACurrency: TStripeCurrency = scGbp): IStripeCharge;
var
  AParams: TStrings;
  AResult: string;
  AJson: TJSONObject;
begin
  Result := TStripeCharge.Create;
  AParams := TStringList.Create;
  try
    AParams.Values['amount'] := IntToStr(AAmountPence);
    AParams.Values['currency'] := CurrencyToString(ACurrency);
    AResult := PostHttp(AToken, C_CHARGES, AParams);
    AJson := TJSONObject.ParseJSONValue(AResult) as TJSONObject;
    CheckForError(AJson);
    Result.LoadFromJson(AJson);
  finally
    AParams.Free;
  end;
end;

function TStripe.CreateChargeForCustomer(ACustID, ADescription: string;
  AAmountPence: integer; const ACurrency: TStripeCurrency): IStripeCharge;
begin
  Result := CreateCharge(ACustID, ADescription, AAmountPence, ACurrency);
end;

function TStripe.CreateHttp: TNetHTTPClient;
begin
  Result := TNetHTTPClient.Create(nil);
  Result.OnAuthEvent := NetHTTPClient1AuthEvent;
end;

function TStripe.CreateToken(ACardNum: string; AExpMonth, AExpYear: integer;
  ACvc: string): string;
var
  AParams: TStrings;
  AResult: string;
  AJson: TJSONObject;
begin
  AParams := TStringList.Create;
  try
    AParams.Values['card[number]'] := ACardNum;
    AParams.Values['card[exp_month]'] := IntToStr(AExpMonth);;
    AParams.Values['card[exp_year]'] := IntToStr(AExpYear);
    AParams.Values['card[cvc]'] := ACvc;
    AResult := PostHttp('', C_TOKENS,AParams);
    AJson := TJSONObject.ParseJSONValue(AResult) as TJSONObject;
    CheckForError(AJson);
    try
      Result := AJson.Values['id'].Value;
    finally
      AJson.Free;
    end;
  finally
    AParams.Free;
  end;
end;

function TStripe.GetCustomer(ACustID: string): IStripeCustomer;
var
  AResult: string;
  AJson: TJSONObject;
begin
  Result := TStripeCustomer.Create;
  AResult := GetHttp(C_CUSTOMERS+'/'+ACustID);
  AJson := TJSONObject.ParseJSONValue(AResult) as TJSONObject;
  try
    Result.LoadFromJson(AJson);
  finally
    AJson.Free;
  end;
end;

function TStripe.GetCustomers: IStripeCusotomerList;
var
  AResult: string;
  AJson: TJSONObject;
begin
  Result := TStripeCustomerList.Create;
  AResult := GetHttp(C_CUSTOMERS);
  AJson := TJSONObject.ParseJSONValue(AResult) as TJSONObject;
  try
    Result.LoadFromJson(AJson);
  finally
    AJson.Free;
  end;
end;

function TStripe.GetHttp(AMethod: string): string;
var
  AHttp: TNetHTTPClient;
  AResponse: IHTTPResponse;
begin
  AHttp := CreateHttp;
  try
    AHttp.CustomHeaders['Authorization'] := 'Bearer '+FSecretKey;
    AResponse := AHttp.Get('https://api.stripe.com/v1/'+AMethod);
    Result := AResponse.ContentAsString
  finally
    AHttp.Free;
  end;
end;

function TStripe.GetLastError: string;
begin
  Result := FLastError;
end;

function TStripe.PostHttp(AToken, AMethod: string; AParams: TStrings): string;
var
  AHttp: TNetHTTPClient;
  AResponse: IHTTPResponse;
begin
  AHttp := CreateHttp;
  try
    if AToken <> '' then
    begin
      if Pos('tok_', AToken) = 1 then AParams.Values['source'] := AToken;
      if Pos('cus_', AToken) = 1 then AParams.Values['customer'] := AToken;
    end;
    AHttp.CustomHeaders['Authorization'] := 'Bearer '+FSecretKey;
    AResponse := AHttp.Post('https://api.stripe.com/v1/'+AMethod, AParams);
    Result := AResponse.ContentAsString
  finally
    AHttp.Free;
  end;
end;

function TStripe.CreateCustomer(AEmail, ADescription: string; ABalancePence: integer): IStripeCustomer;
var
  AParams: TStrings;
  AResult: string;
  AJson: TJSONObject;
begin
  Result := TStripeCustomer.Create;
  AParams := TStringList.Create;
  try
    AParams.Values['email'] := AEmail;
    AParams.Values['description'] := ADescription;
    AParams.Values['account_balance'] := IntToStr(ABalancePence);
    AResult := PostHttp('', C_CUSTOMERS, AParams);
    AJson := TJSONObject.ParseJSONValue(AResult) as TJSONObject;
    CheckForError(AJson);
    Result.LoadFromJson(AJson);
  finally
    AParams.Free;
  end;
end;

procedure TStripe.NetHTTPClient1AuthEvent(const Sender: TObject;
  AnAuthTarget: TAuthTargetType; const ARealm, AURL: string; var AUserName,
  APassword: string; var AbortAuth: Boolean;
  var Persistence: TAuthPersistenceType);
begin
  if AnAuthTarget = TAuthTargetType.Server then
  begin
    AUserName := FSecretKey;
    APassword := '';
  end;
end;

//------------------------------------------------------------------------------

{ TStripeCard }

function TStripeCard.GetAddress1: string;
begin
  Result := FAddress1;
end;

function TStripeCard.GetAddress2: string;
begin
  Result := FAddress2;
end;

function TStripeCard.GetAddressCountry: string;
begin
  Result := FAddressCountry;
end;

function TStripeCard.GetBrand: string;
begin
  Result := FBrand;
end;

function TStripeCard.GetCity: string;
begin
  Result := FCity;
end;

function TStripeCard.GetCountry: string;
begin
  Result := FCountry;
end;

function TStripeCard.GetCvcCheck: string;
begin
  Result := FCvcCheck;
end;

function TStripeCard.GetExpMonth: integer;
begin
  Result :=FExpMonth;
end;

function TStripeCard.GetExpYear: integer;
begin
  Result := FExpYear;
end;

function TStripeCard.GetFunding: string;
begin
  Result := FFunding;
end;

function TStripeCard.GetLast4: string;
begin
  Result := FLast4;
end;

function TStripeCard.GetName: string;
begin
  Result := FName;
end;


function TStripeCard.GetObject: string;
begin
  Result := C_CARD;
end;

function TStripeCard.GetState: string;
begin
  Result := FState;
end;

function TStripeCard.GetZip: string;
begin
  Result := FZip;
end;

//------------------------------------------------------------------------------

{ TStripeBaseObject }

constructor TStripeBaseObject.Create;
begin
  FId := '';
  FObj := GetObject;
end;

function TStripeBaseObject.DateFromJson(AName: string): TDateTime;
var
  ATimestamp: integer;
begin
  ATimestamp := IntFromJson(AName);
  Result := UnixToDateTime(ATimestamp);
end;

function TStripeBaseObject.BoolFromJson(AName: string): Boolean;
begin
  Result := False;
  if FJson.Values[AName] <> nil then
    Result := LowerCase(FJson.Values[AName].Value) = 'true';
end;

function TStripeBaseObject.IntFromJson(AName: string): integer;
begin
  Result := 0;
  if StrFromJson(AName) <> '' then
  Result := StrToIntDef(StrFromJson(AName), 0);
end;

function TStripeBaseObject.GetID: string;
begin
  Result := FId;
end;

function TStripeBaseObject.GetObj: string;
begin
  Result := FObj;
end;

procedure TStripeBaseObject.LoadFromJson(AJson: TJsonObject);
begin
  FJson := AJson;
  FId := StrFromJson('id');
  FObj := StrFromJson('object');
end;

function TStripeBaseObject.StrFromJson(AName: string): string;
begin
  Result := '';
  if FJson.Values[AName] <> nil then
    Result := FJson.Values[AName].Value;
end;

//------------------------------------------------------------------------------

{ TStripeCustomer }

constructor TStripeCustomer.Create;
begin
  FSubscriptions := TStripeSubscriptionList.Create;
  FCurrency := scGbp;
end;

function TStripeCustomer.GetAccountBalance: integer;
begin
  Result := FAccountBalance;
end;

function TStripeCustomer.GetCurrency: TStripeCurrency;
begin
  Result := FCurrency;
end;

function TStripeCustomer.GetDescription: string;
begin
  Result := FDescription;
end;

function TStripeCustomer.GetEmail: string;
begin
  Result := FEmail;
end;

function TStripeCustomer.GetObject: string;
begin
  Result := C_CUSTOMER;
end;

procedure TStripeCustomer.LoadFromJson(AJson: TJsonObject);
begin
  inherited;
  FEmail := StrFromJson('email');
  FCurrency := StringToCurrency(StrFromJson('currency'));
  FAccountBalance := IntFromJson('account_balance');
  FSubscriptions.LoadFromJson(AJson.Values['subscriptions'] as TJSONObject);
end;

procedure TStripeCustomer.SetAccountBalance(const Value: integer);
begin
  FAccountBalance := Value;
end;

procedure TStripeCustomer.SetCurrency(const Value: TStripeCurrency);
begin
  FCurrency := Value;
end;

procedure TStripeCustomer.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TStripeCustomer.SetEmail(const Value: string);
begin
  FEmail := Value;
end;

//------------------------------------------------------------------------------

{ TStripePlan }

function TStripePlan.GetAmountPence: integer;
begin
  Result := FAmountPence;
end;

function TStripePlan.GetCreated: TDateTime;
begin
  Result := FCreated;
end;

function TStripePlan.GetCurrency: TStripeCurrency;
begin
  Result := FCurrency;
end;

function TStripePlan.GetInterval: string;
begin
  Result := FInterval;
end;

function TStripePlan.GetIntervalCount: integer;
begin
  Result := FIntervalCount;
end;

function TStripePlan.GetName: string;
begin
  Result := FName;
end;

function TStripePlan.GetObject: string;
begin
  Result := C_PLAN;
end;

function TStripePlan.GetStatementDescriptor: string;
begin
  Result := FStatementDescriptor;
end;

function TStripePlan.GetTrialPeriodDays: integer;
begin
  Result := FTrialPeriodDays;
end;

procedure TStripePlan.LoadFromJson(AJson: TJsonObject);
begin
  inherited;
  FInterval := StrFromJson('interval');
  FName := StrFromJson('name');
  FCreated := DateFromJson('created');
  FAmountPence := IntFromJson('amount');
  FCurrency :=  StringToCurrency(StrFromJson('currency'));
  FIntervalCount := IntFromJson('interval_count');
  FTrialPeriodDays := IntFromJson('trial_period_days');
  FStatementDescriptor := StrFromJson('statement_descriptor');
end;

//------------------------------------------------------------------------------

{ TStripeSubscription }

constructor TStripeSubscription.Create;
begin
  inherited;
  FPlan := TStripePlan.Create;
end;

function TStripeSubscription.GetCancelledAt: TDateTime;
begin
  Result := FCancelledAt;
end;

function TStripeSubscription.GetCurrentPeriodEnd: TDateTime;
begin
  Result := FCurrentPeriodEnd;
end;

function TStripeSubscription.GetCurrentPeriodStart: TDateTime;
begin
  Result := FCurrentPeriodStart;
end;

function TStripeSubscription.GetCustomer: string;
begin
  Result := FCustomer;
end;

function TStripeSubscription.GetEndedAt: TDateTime;
begin
  Result := FEndedAt;
end;

function TStripeSubscription.GetObject: string;
begin
  Result := C_SUBSCRIPTION;
end;

function TStripeSubscription.GetPlan: IStripePlan;
begin
  Result := FPlan;
end;

function TStripeSubscription.GetQuantity: integer;
begin
  Result := FQuantity;
end;

function TStripeSubscription.GetStart: TDateTime;
begin
  Result := FStart;
end;

function TStripeSubscription.GetStatus: string;
begin
  Result := FStatus;
end;

function TStripeSubscription.GetTaxPercent: single;
begin
  Result := FTaxPercent;
end;

function TStripeSubscription.GetTrialEnd: TDateTime;
begin
  Result := FTrialEnd;
end;

function TStripeSubscription.GetTrialStart: TDateTime;
begin
  Result := FTrialStart;
end;

procedure TStripeSubscription.LoadFromJson(AJson: TJsonObject);
begin
  inherited;
  FPlan.LoadFromJson(AJson.Values['plan'] as TJSONObject);
  FStatus := StrFromJson('status');
end;

//------------------------------------------------------------------------------

{ TStripeBaseObjectList<T> }

function TStripeBaseObjectList.AddObject: IStripeBaseObject;
begin
  Result := CreateObject;
  FItems.Add(TStripeBaseObject(Result));
end;

procedure TStripeBaseObjectList.Clear;
begin
  FItems.Clear;
end;

constructor TStripeBaseObjectList.Create;
begin
  FItems := TList<TStripeBaseObject>.Create;
end;

function TStripeBaseObjectList.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TStripeBaseObjectList.GetItem(index: integer): IStripeBaseObject;
begin
  Result := FItems[index];
end;

procedure TStripeBaseObjectList.LoadFromJson(AJson: TJSONObject);
var
  AArray: TJSONArray;
  ICount: integer;
begin
  Clear;
  if AJson = nil then
    Exit;
  AArray := AJson.Values['data'] as TJSONArray;
  for ICount := 0 to AArray.Count-1 do
  begin
    AddObject.LoadFromJson(AArray.Items[ICount] as TJsonObject);
  end;
end;

//------------------------------------------------------------------------------

{ TStripeCustomerList }

function TStripeCustomerList.CreateObject: IStripeBaseObject;
begin
  Result := TStripeCustomer.Create;
end;

function TStripeCustomerList.GetListID: string;
begin
  Result := C_CUSTOMERS;
end;

//------------------------------------------------------------------------------

{ TStripeSubscriptionList }

function TStripeSubscriptionList.CreateObject: IStripeBaseObject;
begin
  Result := TStripeSubscription.Create;
end;

function TStripeSubscriptionList.GetListID: string;
begin
  Result := C_SUBSCRIPTIONS;
end;

//------------------------------------------------------------------------------

{ TStripeCharge }

function TStripeCharge.GetAmountPence: integer;
begin
  Result := FAmountPence;
end;

function TStripeCharge.GetCurrency: TStripeCurrency;
begin
  Result := FCurrency;
end;

function TStripeCharge.GetCreated: TDatetime;
begin
  Result := FCreated;
end;

function TStripeCharge.GetLiveMode: Boolean;
begin
  Result := FLiveMode;
end;

function TStripeCharge.GetObject: string;
begin
  Result := C_CHARGE;
end;

function TStripeCharge.GetPaid: Boolean;
begin
  Result := FPaid;
end;

function TStripeCharge.GetRefunded: Boolean;
begin
  Result := FRefunded;
end;

function TStripeCharge.GetStatus: string;
begin
  Result := FStatus;
end;

procedure TStripeCharge.LoadFromJson(AJson: TJsonObject);
begin
  inherited;
  FCreated := DateFromJson('created');
  FLiveMode := BoolFromJson('livemode');
  FPaid := BoolFromJson('livemode');
  FStatus := StrFromJson('status');
  FAmountPence := IntFromJson('amount');
  FCurrency := StringToCurrency(StrFromJson('currency'));
  FRefunded := BoolFromJson('refunded');
end;

end.
