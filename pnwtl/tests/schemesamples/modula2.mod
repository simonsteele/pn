MODULE MainProgram;
   FROM ShapeMod IMPORT Shape;
   FROM CircleMod IMPORT Circle, MakeCircle;
   FROM RectangleMod IMPORT Rectangle, MakeRectangle;

VAR
   i: INTEGER;
   scribble: ARRAY[1..2] OF Shape;
   rect: Rectangle;

BEGIN
   (* set up some shape instances *)
   scribble[1] := Shape(MakeRectangle(10,20,5,6));
   scribble[2] := Shape(MakeCircle(15,25,8));

   (*
      *** Note:  I think the above form of the Type Transfer Function has
                 been obsoleted.  Newer form is:
      scribble[1] := SYSTEM.CAST(Shape, MakeRectangle(10,20,5,6));
      scribble[2] := SYSTEM.CAST(Shape, MakeCircle(15,25,8));

      *** Note:  This form also requires an IMPORT statement at the
                 top of this module of the form:
      FROM SYSTEM IMPORT CAST;

      *** Note:  It's possible that a Modula-2 compiler may complain about
                 the type casting here?  I ran the code through the XDS
                 compiler and it went through with no complaints, but
                 the demo version does not allow executables so I didn't
                 get a chance to test it.
   *)


   (* iterate through the shapes and handle polymorphically *)
   FOR i := 1 TO 2 DO
      scribble[i]^.draw(scribble[i]);
      scribble[i]^.rMoveTo(scribble[i], 100, 100);
      scribble[i]^.draw(scribble[i]);
   END;

   (* access a rectangle specific function *)
   rect := MakeRectangle(0,0,15,15);
   rect^.setWidth(rect, 30);
   rect^.draw(rect);

   (* deallocate the shape instances *)
   FOR i := 1 TO 2 DO
      scribble[i]^.disposeThis(scribble[i]);
   END;
   rect^.disposeThis(rect);
END MainProgram.