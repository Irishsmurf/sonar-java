/*
 * SonarQube Java
 * Copyright (C) 2012-2016 SonarSource SA
 * mailto:contact AT sonarsource DOT com
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */
package org.sonar.java.se.checks;

import com.google.common.collect.ImmutableList;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;
import org.sonar.java.se.CheckerContext;
import org.sonar.java.se.ProgramState;
import org.sonar.java.se.constraint.ConstraintManager;
import org.sonar.java.se.constraint.ObjectConstraint;
import org.sonar.java.se.symbolicvalues.SymbolicValue;
import org.sonar.plugins.java.api.semantic.Symbol;
import org.sonar.plugins.java.api.semantic.Type;
import org.sonar.plugins.java.api.tree.Arguments;
import org.sonar.plugins.java.api.tree.ExpressionTree;
import org.sonar.plugins.java.api.tree.IdentifierTree;
import org.sonar.plugins.java.api.tree.MemberSelectExpressionTree;
import org.sonar.plugins.java.api.tree.MethodInvocationTree;
import org.sonar.plugins.java.api.tree.NewClassTree;
import org.sonar.plugins.java.api.tree.ReturnStatementTree;
import org.sonar.plugins.java.api.tree.Tree;
import org.sonar.squidbridge.annotations.NoSqale;
import org.sonar.squidbridge.annotations.RuleTemplate;

import javax.annotation.Nullable;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Rule(
  key = "S3546",
  name = "Resources as defined by user should be closed",
  priority = Priority.BLOCKER,
  tags = {"bug", "cert", "cwe", "denial-of-service", "leak", "security"})
@RuleTemplate
@NoSqale
public class CustomUnclosedResourcesCheck extends SECheck {

  private static final String ARGUMENT_DESCRIPTION = " If the signature is important, then it must be supplied by listing the full qualified names of each argument, coma delimited within parentheses.";

  private enum Status {
    OPENED, CLOSED
  }

  @RuleProperty(
    key = "constructor",
    description = "List of classes (full qualified name) used to create instances that need to be closed." + ARGUMENT_DESCRIPTION)
  public String constructor = "";

  @RuleProperty(
    key = "factoryMethod",
    description = "List of classes (full qualified name#methodName) used to open instances that need to be closed." + ARGUMENT_DESCRIPTION)
  public String factoryMethod = "";

  @RuleProperty(
    key = "openingMethod",
    description = "List of methods (full qualified name#methodName) used to create instances that need to be closed." + ARGUMENT_DESCRIPTION)
  public String openingMethod = "";

  @RuleProperty(
    key = "closingMethod",
    description = "List of methods (full qualified name#methodName) used to close instances that need to be closed." + ARGUMENT_DESCRIPTION)
  public String closingMethod = "";

  private List<ConstructorMatcher> classConstructor;
  private List<MethodMatcher> factoryList;
  private List<MethodMatcher> openingList;
  private List<MethodMatcher> closingList;

  @Override
  public ProgramState checkPreStatement(CheckerContext context, Tree syntaxNode) {
    final PreStatementVisitor visitor = new PreStatementVisitor(context);
    syntaxNode.accept(visitor);
    return visitor.programState;
  }

  @Override
  public ProgramState checkPostStatement(CheckerContext context, Tree syntaxNode) {
    final PostStatementVisitor visitor = new PostStatementVisitor(context);
    syntaxNode.accept(visitor);
    return visitor.programState;
  }

  @Override
  public void checkEndOfExecutionPath(CheckerContext context, ConstraintManager constraintManager) {
    final List<ObjectConstraint> constraints = context.getState().getFieldConstraints(Status.OPENED);
    for (ObjectConstraint constraint : constraints) {
      Tree syntaxNode = constraint.syntaxNode();
      String name = null;
      if (syntaxNode.is(Tree.Kind.NEW_CLASS)) {
        name = ((NewClassTree) syntaxNode).identifier().symbolType().name();
      } else if (syntaxNode.is(Tree.Kind.METHOD_INVOCATION)) {
        name = ((MethodInvocationTree) syntaxNode).symbolType().name();
      }
      if (name != null) {
        context.reportIssue(syntaxNode, this, "Close this \"" + name + "\".");
      }
    }
  }

  boolean isOpeningResource(MethodInvocationTree syntaxNode) {
    for (MethodMatcher matcher : openingMethods()) {
      if (matcher.matches(syntaxNode)) {
        return true;
      }
    }
    return false;
  }

  List<ConstructorMatcher> constructorClasses() {
    if (classConstructor == null) {
      if (constructor.length() > 0) {
        classConstructor = ImmutableList.of(new ConstructorMatcher(constructor));
      } else {
        classConstructor = ImmutableList.of();
      }
    }
    return classConstructor;
  }

  List<MethodMatcher> factoryMethods() {
    if (factoryList == null) {
      factoryList = new ArrayList<>();
      if (factoryMethod.length() > 0) {
        factoryList = ImmutableList.of(new MethodMatcher(factoryMethod));
      } else {
        factoryList = ImmutableList.of();
      }
    }
    return factoryList;
  }

  List<MethodMatcher> openingMethods() {
    if (openingList == null) {
      if (openingMethod.length() > 0) {
        openingList = ImmutableList.of(new MethodMatcher(openingMethod));
      } else {
        openingList = ImmutableList.of();
      }
    }
    return openingList;
  }

  List<MethodMatcher> closingMethods() {
    if (closingList == null) {
      if (closingMethod.length() > 0) {
        closingList = ImmutableList.of(new MethodMatcher(closingMethod));
      } else {
        closingList = ImmutableList.of();
      }
    }
    return closingList;
  }

  private abstract class AbstractStatementVisitor extends CheckerTreeNodeVisitor {

    protected AbstractStatementVisitor(ProgramState programState) {
      super(programState);
    }

    protected void closeResource(@Nullable final SymbolicValue target) {
      if (target != null) {
        ObjectConstraint oConstraint = ObjectConstraint.constraintWithStatus(programState, target, Status.OPENED);
        if (oConstraint != null) {
          programState = programState.addConstraint(target.wrappedValue(), oConstraint.withStatus(Status.CLOSED));
        }
      }
    }
  }

  private class PreStatementVisitor extends AbstractStatementVisitor {

    protected PreStatementVisitor(CheckerContext context) {
      super(context.getState());
    }

    @Override
    public void visitNewClass(NewClassTree syntaxNode) {
      closeArguments(syntaxNode.arguments(), 0);
    }

    @Override
    public void visitMethodInvocation(MethodInvocationTree syntaxNode) {
      if (isOpeningResource(syntaxNode)) {
        final SymbolicValue instanceValue = programState.peekValue();
        programState = programState.addConstraint(instanceValue, new ObjectConstraint(false, false, syntaxNode, Status.OPENED));
      } else {
        closeArguments(syntaxNode.arguments(), 1);
      }
    }

    @Override
    public void visitReturnStatement(ReturnStatementTree syntaxNode) {
      SymbolicValue currentVal = programState.peekValue();
      if (currentVal != null) {
        final ExpressionTree expression = syntaxNode.expression();
        if (expression != null) {
          if (expression.is(Tree.Kind.IDENTIFIER)) {
            final IdentifierTree identifier = (IdentifierTree) expression;
            currentVal = programState.getValue(identifier.symbol());
          } else {
            currentVal = programState.peekValue();
          }
          closeResource(currentVal);
        }
      }
    }

    private void closeArguments(final Arguments arguments, int stackOffset) {
      final List<SymbolicValue> values = programState.peekValues(arguments.size() + stackOffset);
      final List<SymbolicValue> argumentValues = values.subList(stackOffset, values.size());
      for (SymbolicValue target : argumentValues) {
        closeResource(target);
      }
    }
  }

  private class PostStatementVisitor extends AbstractStatementVisitor {

    protected PostStatementVisitor(CheckerContext context) {
      super(context.getState());
    }

    @Override
    public void visitNewClass(NewClassTree syntaxNode) {
      if (isCreatingResource(syntaxNode)) {
        final SymbolicValue instanceValue = programState.peekValue();
        programState = programState.addConstraint(instanceValue, new ObjectConstraint(false, false, syntaxNode, Status.OPENED));
      }
    }

    @Override
    public void visitMethodInvocation(MethodInvocationTree syntaxNode) {
      if (isClosingResource(syntaxNode)) {
        final ExpressionTree targetExpression = ((MemberSelectExpressionTree) syntaxNode.methodSelect()).expression();
        if (targetExpression.is(Tree.Kind.IDENTIFIER)) {
          final IdentifierTree identifier = (IdentifierTree) targetExpression;
          closeResource(programState.getValue(identifier.symbol()));
        } else {
          closeResource(programState.peekValue());
        }
      } else if (isCreatingResource(syntaxNode)) {
        final SymbolicValue instanceValue = programState.peekValue();
        programState = programState.addConstraint(instanceValue, new ObjectConstraint(false, false, syntaxNode, Status.OPENED));
      }
    }

    private boolean isCreatingResource(NewClassTree syntaxNode) {
      for (ConstructorMatcher matcher : constructorClasses()) {
        if (matcher.matches(syntaxNode)) {
          return true;
        }
      }
      return false;
    }

    private boolean isCreatingResource(MethodInvocationTree syntaxNode) {
      for (MethodMatcher matcher : factoryMethods()) {
        if (matcher.matches(syntaxNode)) {
          return true;
        }
      }
      return false;
    }

    private boolean isClosingResource(MethodInvocationTree methodInvocation) {
      for (MethodMatcher matcher : closingMethods()) {
        if (matcher.matches(methodInvocation)) {
          return true;
        }
      }
      return false;
    }
  }

  private abstract static class ArgumentMatcher {

    private static final Pattern ARGUMENT_PATTERN = Pattern.compile("(\\w+[\\.\\w+]*(?:\\[\\])?)([,\\)])?");

    private final String className;
    private final List<String> arguments;

    protected ArgumentMatcher(String descriptor) {
      Matcher matcher = pattern().matcher(descriptor);
      if (!matcher.find()) {
        throw new IllegalArgumentException(errorMessage(descriptor));
      }
      className = matcher.group(1);
      int n = initializeOtherArguments(matcher);
      if ("(".equals(matcher.group(n))) {
        arguments = new ArrayList<>();
        String remainder = descriptor.substring(matcher.group().length());
        if (!")".equals(remainder)) {
          matcher = ARGUMENT_PATTERN.matcher(remainder);
          while (matcher.find()) {
            arguments.add(matcher.group(1));
          }
          if (!matcher.hitEnd()) {
            throw new IllegalArgumentException(errorMessage(descriptor));
          }
        }
      } else {
        arguments = null;
      }
    }

    protected boolean checkArguments(Arguments constructorArguments) {
      if (arguments != null) {
        Iterator<String> iterator = arguments.iterator();
        for (ExpressionTree expressionTree : constructorArguments) {
          Type argumentType = expressionTree.symbolType();
          if (!(iterator.hasNext() && argumentType.isSubtypeOf(iterator.next()))) {
            return false;
          }
        }
      }
      return true;
    }

    protected boolean checkType(Type type) {
      return type.isSubtypeOf(className);
    }

    protected void appendArguments(StringBuilder buffer) {
      if (arguments != null) {
        char delimiter = '(';
        for (String argument : arguments) {
          buffer.append(delimiter);
          buffer.append(argument);
          delimiter = ',';
        }
        buffer.append(')');
      }
    }

    protected void appendClass(StringBuilder buffer) {
      buffer.append(className);
    }

    protected abstract Pattern pattern();

    protected abstract int initializeOtherArguments(Matcher matcher);

    protected abstract String errorMessage(String descriptor);
  }

  private static class ConstructorMatcher extends ArgumentMatcher {

    private static final Pattern CLASS_PATTERN = Pattern.compile("^(\\w+[\\.\\w+]*(?:\\[\\])?)([\\(])?");

    ConstructorMatcher(String constructorDescriptor) {
      super(constructorDescriptor);
    }

    @Override
    protected Pattern pattern() {
      return CLASS_PATTERN;
    }

    @Override
    protected int initializeOtherArguments(Matcher matcher) {
      return 2;
    }

    @Override
    protected String errorMessage(String descriptor) {
      return "Illegal constructor specification: " + descriptor;
    }

    boolean matches(NewClassTree syntaxNode) {
      Type type = syntaxNode.identifier().symbolType();
      return checkType(type) && checkArguments(syntaxNode.arguments());
    }

    @Override
    public String toString() {
      StringBuilder buffer = new StringBuilder();
      appendClass(buffer);
      appendArguments(buffer);
      return buffer.toString();
    }
  }

  private static class MethodMatcher extends ArgumentMatcher {

    private static final Pattern METHOD_PATTERN = Pattern.compile("^(\\w+[\\.\\w+]*(?:\\[\\])?)#(\\w+)([\\(])?");

    private String methodName;

    MethodMatcher(String methodDescriptor) {
      super(methodDescriptor);
    }

    @Override
    protected Pattern pattern() {
      return METHOD_PATTERN;
    }

    @Override
    protected int initializeOtherArguments(Matcher matcher) {
      methodName = matcher.group(2);
      return 3;
    }

    @Override
    protected String errorMessage(String descriptor) {
      return "Illegal method specification: " + descriptor;
    }

    boolean matches(MethodInvocationTree syntaxNode) {
      Symbol symbol = syntaxNode.symbol();
      if (syntaxNode.methodSelect().is(Tree.Kind.MEMBER_SELECT)) {
        Type type = ((MemberSelectExpressionTree) syntaxNode.methodSelect()).expression().symbolType();
        return methodName.equals(symbol.name()) && checkType(type) && checkArguments(syntaxNode.arguments());
      }
      return false;
    }

    @Override
    public String toString() {
      StringBuilder buffer = new StringBuilder();
      appendClass(buffer);
      buffer.append('#');
      buffer.append(methodName);
      appendArguments(buffer);
      return buffer.toString();
    }
  }

}
