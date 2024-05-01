package tofu.syntax

package object streams {

  object broadcast extends BroadcastSyntax

  object chunks extends ChunkSyntax

  object combineK extends CombineKSyntax

  object compile extends CompileSyntax

  object emits extends EmitsSyntax

  object evals extends EvalsSyntax

  object filter extends StreamFilterSyntax

  object merge extends MergeSyntax

  object pace extends PaceSyntax

  object parFlatten extends ParFlattenSyntax

  object region extends RegionSyntax

  object temporal extends TemporalSyntax

  object all
      extends BroadcastSyntax with ChunkSyntax with CombineKSyntax with CompileSyntax with EmitsSyntax with EvalsSyntax
      with StreamFilterSyntax with MergeSyntax with PaceSyntax with ParFlattenSyntax with RegionSyntax
      with TemporalSyntax
}
