unit Pkg.Json.ThreadingEx;

interface

uses
  SysUtils,
  Threading;

type
  TAction<T> = reference to procedure(const arg: T);

  TTaskContinuationOptions = (NotOnCompleted, NotOnFaulted, NotOnCanceled, OnlyOnCompleted, OnlyOnFaulted, OnlyOnCanceled);

  ITaskEx = interface(ITask)
    ['{3AE1A614-27AA-4B5A-BC50-42483650E20D}']
    function GetExceptObj: Exception;
    function GetStatus: TTaskStatus;
    function ContinueWith(const continuationAction: TAction<ITaskEx>; continuationOptions: TTaskContinuationOptions): ITaskEx;
    property ExceptObj: Exception read GetExceptObj;
    property Status: TTaskStatus read GetStatus;
  end;

  TTaskEx = class(TTask, ITaskEx)
  private
    fExceptObj: Exception;
    function GetExceptObj: Exception;
  protected
    function ContinueWith(const continuationAction: TAction<ITaskEx>; continuationOptions: TTaskContinuationOptions): ITaskEx;
  public
    destructor Destroy; override;

    class function Run(const action: TProc): ITaskEx; static;
    class function QueueMainThread(aDelay: Integer; const action: TProc): ITaskEx; overload;
  end;

implementation

uses
  Classes;

{ TTaskEx }

function TTaskEx.ContinueWith(const continuationAction: TAction<ITaskEx>; continuationOptions: TTaskContinuationOptions): ITaskEx;
begin
  Result := TTaskEx.Run(
      procedure
    var
      task: ITaskEx;
      doContinue: Boolean;
    begin
      task := Self;
      if not IsComplete then
        DoneEvent.WaitFor;
      fExceptObj := GetExceptionObject;
      case continuationOptions of
        NotOnCompleted:
          doContinue := GetStatus <> TTaskStatus.Completed;
        NotOnFaulted:
          doContinue := GetStatus <> TTaskStatus.Exception;
        NotOnCanceled:
          doContinue := GetStatus <> TTaskStatus.Canceled;
        OnlyOnCompleted:
          doContinue := GetStatus = TTaskStatus.Completed;
        OnlyOnFaulted:
          doContinue := GetStatus = TTaskStatus.Exception;
        OnlyOnCanceled:
          doContinue := GetStatus = TTaskStatus.Canceled;
      else
        doContinue := False;
      end;
      if doContinue then
        continuationAction(task);
    end);
end;

destructor TTaskEx.Destroy;
begin
  fExceptObj.Free;
  inherited;
end;

function TTaskEx.GetExceptObj: Exception;
begin
  Result := fExceptObj;
end;

class function TTaskEx.QueueMainThread(aDelay: Integer; const action: TProc): ITaskEx;
begin
  Result := Run(
    procedure
    begin
      TThread.Sleep(aDelay);
      TThread.Queue(nil,
        procedure
        begin
          action;
        end
      );
    end);
end;

class function TTaskEx.Run(const action: TProc): ITaskEx;
var
  task: TTaskEx;
begin
  task := TTaskEx.Create(nil, TNotifyEvent(nil), action, TThreadPool.Default, nil);
  Result := task.Start as ITaskEx;
end;

end.
