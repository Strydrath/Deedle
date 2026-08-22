namespace Deedle.Virtual

open System
open Deedle
open Deedle.Indices.Virtual
open Deedle.Vectors
open Deedle.Vectors.Virtual
open Deedle.Internal
open Deedle.VectorHelpers

/// Describes how the row index of a virtual frame is stored.
type VirtualRowIndexKind =
  | OrderedVirtual
  | OrdinalVirtual
  | LinearOrOther

/// Diagnostics for virtual frames (Big Deedle tooling).
type VirtualFrameDiagnostics =
  static member GetRowIndexKind(frame: Frame<'R, 'C>) =
    match frame.RowIndex with
    | :? VirtualOrdinalIndex -> VirtualRowIndexKind.OrdinalVirtual
    | :? VirtualOrderedIndex<'R> -> VirtualRowIndexKind.OrderedVirtual
    | _ -> VirtualRowIndexKind.LinearOrOther

  static member IsVirtualRowIndex(frame: Frame<'R, 'C>) =
    match VirtualFrameDiagnostics.GetRowIndexKind frame with
    | VirtualRowIndexKind.LinearOrOther -> false
    | _ -> true

  static member IsVirtualColumn(frame: Frame<_, 'C>, column: 'C when 'C : equality) =
    frame.GetAllColumns<obj>(ConversionKind.Flexible)
    |> Seq.tryPick (fun (KeyValue(k, s)) ->
        if k = column then Some (VirtualFrameDiagnostics.isVirtualVector s.Vector) else None)
    |> Option.defaultValue false

  static member Describe(frame: Frame<'R, 'C>) =
    let kind =
      match VirtualFrameDiagnostics.GetRowIndexKind frame with
      | OrderedVirtual -> "ordered virtual"
      | OrdinalVirtual -> "ordinal virtual (0..N-1)"
      | LinearOrOther -> "linear / materialized"
    sprintf "rows=%d, rowIndex=%s, columns=%d" frame.RowCount kind frame.ColumnCount

  /// Scheme id from the virtual row-index source, when present (e.g. `"csv-file"`, instrumented test ids).
  static member TryGetRowIndexSchemeId(frame: Frame<'R, 'C>) =
    match frame.RowIndex with
    | :? VirtualOrderedIndex<'R> as idx -> Some idx.Source.AddressingSchemeID
    | :? VirtualOrdinalIndex as idx -> Some idx.Source.AddressingSchemeID
    | _ -> None

  static member IsVirtual(frame: Frame<'R, 'C>) = VirtualFrameDiagnostics.IsVirtualRowIndex frame

  static member internal isVirtualVector (v: IVector) =
    let rec unwrap (vec: IVector) =
      match vec with
      | :? VirtualVector<_> -> true
      | :? IWrappedVector<_> as w -> unwrap (w.UnwrapVector())
      | _ -> false
    unwrap v
