module Renderer

    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Browser
    open Elmish
    open Elmish.React

    
    /// X,Y SVG coordinates
    type Coords =
        {
            X : float
            Y : float
        }

    /// model type for a draggable rect
    type Rectangle =
        {
            Pos: Coords
            LastMousePos : Coords
            IsDragging : bool
        }


    type Model = Rectangle list


    type Msg =
        | StartDragging of index : int * pageX : float * pageY : float
        | Dragging of index : int * pageX : float * pageY : float
        | EndDragging of index : int


    let init () =
        let initPos = {X=0. ; Y=0.}
        [
            { 
                Pos={X = 50. ; Y = 50.}
                LastMousePos = initPos
                IsDragging = false
            }
            { 
                Pos ={X=150. ; Y=150.}
                LastMousePos = initPos
                IsDragging = false
            }
            { 
                Pos ={X=100. ; Y=100.}
                LastMousePos = initPos
                IsDragging = false
            }
        ]
        , Cmd.none


    let update (msg : Msg) (currentModel : Model)  =
        match msg with
        | StartDragging (rank, pageX, pageY) ->
            currentModel
            |> List.mapi (fun index rect ->
                if rank <> index then
                    rect
                else
                    { rect with
                        LastMousePos =
                            {
                                X = pageX
                                Y = pageY
                            }
                        IsDragging = true
                    }
            )
            , Cmd.none

        | Dragging (rank, mouseX, mouseY) ->
            currentModel
            |> List.mapi (fun index rect ->
                if rank <> index then
                    rect
                else
                    let xDiff = mouseX - rect.LastMousePos.X 
                    let yDiff = mouseY - rect.LastMousePos.Y 
                    { rect with
                        Pos = {
                            X = rect.Pos.X + xDiff
                            Y = rect.Pos.Y + yDiff
                        }
                        LastMousePos = {
                            X = mouseX
                            Y = mouseY
                            }
                    }
            )
            , Cmd.none
    
        | EndDragging rank ->
            currentModel
            |> List.mapi (fun index rect ->
                if rank <> index then 
                    rect
                else
                    { rect with
                        IsDragging = false 
                    }
            )
            , Cmd.none

    /// inputs needed to render a rect
    type RenderCircleProps =
        {
            Rectangle : Rectangle
            Index : int
            Dispatch : Dispatch<Msg>
            key : string
        }


    let renderCircle =
        FunctionComponent.Of(
            fun (props : RenderCircleProps) ->
                let handleMouseMove =
                    Hooks.useRef(fun (ev : Types.Event) ->
                        let ev = ev :?> Types.MouseEvent

                        Dragging (props.Index, ev.pageX, ev.pageY)
                        |> props.Dispatch
                    )

                let color =
                    if props.Rectangle.IsDragging then
                        "green" 
                    else
                        "lightgrey"
                printfn "Rendering %d as %s" props.Index color
                rect
                    [ 
                        OnMouseUp (fun ev -> 
                            document.removeEventListener("mousemove", handleMouseMove.current)
                            EndDragging props.Index
                            |> props.Dispatch
                        )
                        OnMouseDown (fun ev -> 
                            StartDragging (props.Index, ev.pageX, ev.pageY)
                            |> props.Dispatch
                            document.addEventListener("mousemove", handleMouseMove.current)
                        )
                        X props.Rectangle.Pos.X
                        Y props.Rectangle.Pos.Y
                        // R 25.
                        SVGAttr.Width 75.
                        SVGAttr.Height 100.
                        SVGAttr.Fill color
                        SVGAttr.Stroke color
                        SVGAttr.StrokeWidth 1
                    ]
                    [ ]
        , "Rectangle"
        , equalsButFunctions
        )


    let view (model : Model) (dispatch : Msg -> unit) =
        let handleSvgMouseEvent ev = ()
             
        let rectangles =
            model
            |> List.mapi (fun index rect ->
                renderCircle 
                    {
                        Rectangle = rect
                        Index = index
                        Dispatch = dispatch
                        key = "rect-" + string index
                    }
            )
           
        
        svg [
                Style 
                    [
                        Border "1px solid green"
                        Height "500px"
                        Width "calc(100% - 20px)"
                        Margin "10px"
                    ]
            ]
            rectangles


    // App
    Program.mkProgram init update view
    |> Program.withReactSynchronous "app"
    |> Program.run

