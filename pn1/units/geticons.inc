Type
  TIconKind=(ShellSmall, ShellLarge);

{ find the count'th occurence of the substring,
  if count<0 then look from the back }
function posn(const s,t:string; count:integer):integer;
var
  i,h,last: integer;
  u: string;
begin
  u:=t;
  if count>0 then begin
    result:=length(t);
    for i:=1 to count do begin
      h:=pos(s,u);
      if h>0 then
        u:=copy(u,pos(s,u)+1,length(u))
      else begin
        u:='';
        inc(result);
        end;
      end;
    result:=result-length(u);
    end
  else if count<0 then begin
    last:=0;
    for i:=length(t) downto 1 do begin
      u:=copy(t,i,length(t));
      h:=pos(s,u);
      if (h<>0) and (h+i<>last) then begin
        last:=h+i-1;
        inc(count);
        if count=0 then BREAK;
        end;
      end;
    if count=0 then result:=last
               else result:=0;
    end
  else
    result:=0;
end;

{$HINTS OFF}
function GetIconFromType(const typename : string; kind : TIconKind) : HIcon;
var
  s,t:string;
  v, w : HICON;
  x,y,i:integer;
  reg : tRegistry;
begin { GetIconFromType }
  result:=0;
  s:=typename+'\DefaultIcon'+#0;
  reg := tRegistry.Create;
  reg.RootKey := HKEY_CLASSES_ROOT;
{  if RegOpenKeyEx(HKEY_CLASSES_ROOT,@s[1],0,KEY_READ,h) = error_success then begin}
  if reg.OpenKey(s, False) = True then
  Begin
    x:=255;
    Try
       //Get the Icon File...
       t := reg.ReadString('');
       // Select only the first 256 characters.
       s := copy(t,1,x);
       // Get just the filename, ignore the index.
       t := copy(s,1,pos(',',s)-1)+#0;
       // Get the icon index...
       s := copy(s,pos(',',s)+1,length(s));
       // Convert s to an integer.
       if pos('%', s) = 0 then
       begin
         i := 0;
         try
            i:=strtoint(s);
         except
         end; { try }
         case kind of
            ShellSmall: y := ExtractIconEx(@t[1],i,v,w,1);
            ShellLarge: y := ExtractIconEx(@t[1],i,w,v,1);
         end; { case kind of }
         result := w;
       end else
         result := 0;
    Except
    End;
  end;
  reg.Free;
end; { GetIconFromType }
{$HINTS ON}

function GetIconIndirect(const filename:string; kind: TIconKind) : HIcon;
(* crawl through the registry to find the icon attached to a given
   file extension *)
var
  s,
  t   : string;
  x   : integer;
  reg : tRegistry;
begin { GetIconIndirect }
  result:=0;
  s := filename+#0;
  reg := tRegistry.Create;
  reg.Rootkey := HKEY_CLASSES_ROOT;
  if reg.OpenKey(s, False) Then
  begin
    s := #0;
    x := 255;
    setlength(t,255);
    Try
      t := reg.ReadString('');
      result := GetIconFromType(copy(t, 1, x-1), kind)
      {if RegQueryValueEx(h,@s[1],NIL,@y,@t[1],@x)=error_success then}
    except
    end;
  end;
  reg.Free;
end;
