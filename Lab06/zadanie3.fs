// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open System.Drawing
open System.Windows.Forms
open System.IO

let mainForm = new Form(Width = 620, Height = 450, Text = "Pie Chart")
let menu = new ToolStrip()
let btnOpen = new ToolStripButton("Open")
let btnSave = new ToolStripButton("Save", Enabled = false)
ignore(menu.Items.Add(btnOpen))
ignore(menu.Items.Add(btnSave))
let boxChart = new PictureBox(BackColor = Color.White, Dock = 
        DockStyle.Fill, SizeMode = PictureBoxSizeMode.CenterImage)
mainForm.Controls.Add(menu)
mainForm.Controls.Add(boxChart)
// mainForm.Show() // na razie i tak nic nie ma w oknie
// TODO: Rysowanie wykresu i interakcje z GUI

let rnd = new Random()
let randomBrush() =
    let r, g, b = rnd.Next(256), rnd.Next(256), rnd.Next(256)
    new SolidBrush(Color.FromArgb(r,g,b))

let fnt = new Font("Times New Roman", 11.0f)
let centerX, centerY = 300.0, 200.0
let labelDistance = 150.0

let convertDataRow(csvLine:string) =
    let cells = List.ofSeq(csvLine.Split(','))
    match cells with
    | title::number::_ -> //co najmniej dwa elelmenty
        let parsedNumber = Int32.Parse(number)
        (title, parsedNumber)
    | _ -> failwith "Incorrect data format!"
;;

let rec processLines(lines) =
    match lines with
    | [] -> []
    | currentLine::remaining ->
        let parsedLine = convertDataRow(currentLine)
        let parsedRest = processLines(remaining)
        parsedLine :: parsedRest;;

let rec calculateSum(rows) =
    match rows with
    | [] -> 0
    | (_, value)::tail ->
        let remainingSum = calculateSum(tail)
        value + remainingSum
;;


let testData = processLines ["Test1,123"; "Test2,456"];;
let lines = List.ofSeq(File.ReadAllLines(@"c:\Users\Giin\Desktop\Studies\Funkcyjne laborki\Lab06\dane.csv"));;
let data = processLines(lines);;
let sum = float(calculateSum(data));;

for (title, value) in data do
    let percentage = int((float(value)) / sum * 100.0)
    Console.WriteLine("{0,-18} - {1,8} ({2}%)",title, value, percentage)
;;

let drawPieSegment(gr:Graphics, title, startAngle, occupiedAngle) =
    let br = randomBrush()
    gr.FillPie(br, 170, 70, 260, 260, startAngle, occupiedAngle)
    br.Dispose()

let intToString (x:int) = string x



let drawLabel(gr:Graphics, title, startAngle, angle) =
    let lblAngle = float(startAngle + angle/2)
    let ra = Math.PI * 2.0 * lblAngle / 360.0
    let x = centerX + labelDistance * cos(ra)
    let y = centerY + labelDistance * sin(ra)
    let x1 = centerX + 50.0 * cos(ra)
    let y1 = centerY + 50.0 * sin(ra)
    let x2 = centerX + 80.0 * cos(ra)
    let y2 = centerY + 80.0 * sin(ra)
    let size = gr.MeasureString(title, fnt)
    let rc = new PointF(float32(x) - size.Width / 2.0f, float32(y) - size.Height / 2.0f)
    let rx = new PointF(float32(x1) - size.Width / 2.0f, float32(y1) - size.Height / 2.0f)
    let rz = new PointF(float32(x2) - size.Width / 2.5f , float32(y2) - size.Height / 2.5f )
    gr.DrawString(title, fnt, Brushes.Black, new RectangleF(rc, size))

    for (tytul, value) in data do 
        let procent = int((float(value)) / sum * 100.0)
        if(tytul = title ) then
            gr.DrawString(intToString procent + "%", fnt, Brushes.Black, new RectangleF(rx, size))
    for (tytul, value) in data do 
          if(tytul = title ) then
              gr.DrawString(intToString value, fnt, Brushes.Black, new RectangleF(rz, size))


let drawStep(drawingFunc, gr:Graphics, sum, data) =
    let rec drawStepUtil(data, angleSoFar) =
        match data with
        | [] -> ()
        | [title, value] ->
            let angle = 360 - angleSoFar
            drawingFunc(gr, title, angleSoFar, angle)
        | (title, value)::tail ->
            let angle = int(float(value) / sum * 360.0)
            drawingFunc(gr, title, angleSoFar, angle)
            drawStepUtil(tail, angleSoFar + angle)
    drawStepUtil(data, 0)

let drawChart(file) =
    let lines = List.ofSeq(File.ReadAllLines(file))
    let data = processLines(lines)
    let sum = float(calculateSum(data))
    let pieChart = new Bitmap(600, 400)
    let gr = Graphics.FromImage(pieChart)
    gr.Clear(Color.White)
    drawStep(drawPieSegment, gr, sum, data)
    drawStep(drawLabel, gr, sum, data)
    gr.Dispose()
    pieChart

let openAndDrawChart(e) =
    let dlg = new OpenFileDialog(Filter="CSV Files|*.csv")
    if (dlg.ShowDialog() = DialogResult.OK) then
        let pieChart = drawChart(dlg.FileName)
        boxChart.Image <- pieChart
        btnSave.Enabled <- true
    
let saveDrawing(e) =
    let dlg = new SaveFileDialog(Filter="PNG Files|*.png")
    if (dlg.ShowDialog() = DialogResult.OK) then
        boxChart.Image.Save(dlg.FileName)

[<STAThread>]
do
    btnOpen.Click.Add(openAndDrawChart)
    btnSave.Click.Add(saveDrawing)
    Application.Run(mainForm)